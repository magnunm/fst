/// Regular expressions string matching by converting the regular expression to
/// a NFA (non-deterministic finite automaton).
///
/// Works by first converting the regular expression to postfix notation and
/// then applying Thompson's construction to that expression.
/// The NFA is represented by states in a state register, to simulate the
/// NFA we need this and the id of the start state of the NFA.
use std::str;

mod nfa;

/// A regular expression string, and functions to match a string to it.
pub struct Regex<'a> {
    pub regex: &'a str,

    literal_string_head: &'a str,
    literal_string_tail: &'a str,  // TODO
    // The NFA constructed from the regex and used internally for matching.
    nfa: nfa::NFA<'a>,
    // Whether or not the regex started with a caret (^) and/or ends with a
    // dollar ($).
    from_start: bool,
    until_end: bool,
}

impl<'a> Regex<'a> {
    /// Create a new regex object given the regex string.
    ///
    /// This will construct the NFA needed to do the matching against the
    /// regex string.
    pub fn new(regex: &str) -> Result<Regex, &'static str> {
        let starts_with_caret = regex.chars().next() == Some('^');
        let ends_with_dollar = regex.chars().rev().next() == Some('$');

        // The NFA regex matching does not implemet the caret or dollar,
        // so they are removed from the postifx regex used to construct the NFA.
        let mut start_index_stripped_regex: usize = 0;
        let mut end_index_stripped_regex: usize = regex.len();
        if starts_with_caret {
            start_index_stripped_regex = 1;
        }
        if ends_with_dollar {
            end_index_stripped_regex -= 1;
        }

        let stripped_regex = &regex[start_index_stripped_regex..end_index_stripped_regex];

        let non_literal_range = non_literal_range(stripped_regex);
        let nfa_regex = &stripped_regex[non_literal_range.0..non_literal_range.1];
        let nfa = nfa::regex_to_nfa(nfa_regex)?;

        let literal_string_head = &stripped_regex[0..non_literal_range.0];

        Ok(Regex {
            regex,
            literal_string_head,
            literal_string_tail: "",
            nfa,
            from_start: starts_with_caret,
            until_end: ends_with_dollar
        })
    }

    /// Match the regex to a substring of `input`
    ///
    /// Handles the caret (^) and dollar ($) meta character
    /// functionality. If caret is set it will only match a substring
    /// starting from the beginning of the input. If not it will check
    /// all possible start positions as a possible match start. If
    /// dollar is set it will require that the end of the substring
    /// match is the end of the input.  The matching once a start
    /// position is chosen is handled by the NFA created from the
    /// regex string.  Returns the char byte index of the char where
    /// the matching substring starts and the first char after it.
    pub fn match_substring(&self, input: &str) -> (usize, usize) {
        let mut result = (0, 0);

        if self.literal_string_head != "" {
            if let Some(byte_index_start_literal) = input.find(self.literal_string_head) {
                let byte_index_end_literal = byte_index_start_literal + self.literal_string_head.len();

                let input_for_nfa = &input[byte_index_end_literal..];

                if let Some((_, relative_match_end)) = self.nfa_match_substring(input_for_nfa, true) {
                    result = (byte_index_start_literal,
                              byte_index_end_literal + relative_match_end);
                }
                else {
                    return (0, 0);
                }
            }
        }
        else {
            result = self.nfa_match_substring(input, self.from_start).unwrap_or((0, 0));
        }

        if self.until_end && result.1 != input.len() {
            return (0, 0);
        }

        return result;
    }

    fn nfa_match_substring(&self, input: &str, from_start: bool) -> Option<(usize, usize)> {
        if from_start {
            if let Some(match_end_byte_index) = self.nfa_match_substring_from_start(input) {
                return Some((0, match_end_byte_index));
            }
            return None;
        }

        for (byte_index, _) in input.char_indices() {
            if let Some(relative_match_end) = self.nfa_match_substring_from_start(&input[byte_index..]) {
                return Some((byte_index, byte_index + relative_match_end));
            }
        }

        return None;
    }

    fn nfa_match_substring_from_start(&self, input: &str) -> Option<usize> {
        return self.nfa.simulate(
            input,
            true
        );
    }
}

fn non_literal_range(regex: &str) -> (usize, usize) {
    let mut start_index_non_literal: usize = 0;
    let mut end_index_non_literal: usize = regex.len();  // TODO

    let mut stripped_regex_char_indices = regex.char_indices();
    let mut previous_char_size_bytes: usize = 0;

    while let Some((char_byte_index, character)) = stripped_regex_char_indices.next() {
        start_index_non_literal = char_byte_index;

        if not_part_of_regex_literal(character) {
            if is_regex_operator(character) {
                // These work on a single character, excluding
                // groupings and alterations which are handled above.
                start_index_non_literal -= previous_char_size_bytes;
            }
            if character == '|' {
                // Finding the alteration before any gouping means any
                // potential literal string head found this far is not
                // valid. This since it is alterated with some other
                // expression which might not contain the literal
                // string.
                start_index_non_literal = 0;
            }

            break;
        }

        previous_char_size_bytes = character.len_utf8();  // TODO: UTF-16
    }

    return (start_index_non_literal, end_index_non_literal)
}

fn not_part_of_regex_literal(c: char) -> bool {
    // TODO: Backslash can be handled better, the string is
    // still literal but the backslash should be removed
    // in the literal search
    ['\\', '(', ')', '|'].contains(&c) ||
        is_regex_operator(c) ||
        is_regex_character_class(c)
}

fn is_regex_operator(c: char) -> bool {
    ['+', '?', '*', '|'].contains(&c)
}

fn is_regex_character_class(c: char) -> bool {
    ['[', ']', '.'].contains(&c)
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_substring_matching() -> Result<(), &'static str> {
        // Any string that ends in `a` + number or is any ordered selection
        // from the characters 教育漢字 but nothing more.
        let regex = Regex::new("^.*a[0-9]+|教?育?漢?字?$")?;

        assert_eq!(regex.match_substring("lorem ipsum a2021"),
                   (0, "lorem ipsum a2021".len()));
        assert_eq!(regex.match_substring("a2021"),
                   (0, "a2021".len()));
        assert_eq!(regex.match_substring("教漢"),
                   (0, "教漢".len()));
        assert_eq!(regex.match_substring("abc"),
                   (0, 0));
        Ok(())
    }

    #[test]
    fn test_regex_substring_matching_with_head_literal() -> Result<(), &'static str> {
        let regex = Regex::new("Testcase[0-9]+")?;

        assert_eq!(regex.match_substring("Testcase1"),
                   (0, "Testcase1".len()));
        assert_eq!(regex.match_substring("This is Testcase2"),
                   ("This is ".len(), "This is Testcase2".len()));
        assert_eq!(regex.match_substring("Testcase"),  // Missing the number
                   (0, 0));
        assert_eq!(regex.match_substring("12"),  // Missing the literal
                   (0, 0));
        Ok(())
    }

    #[test]
    fn test_regex_substring_matching_pure_literal() -> Result<(), &'static str> {
        let regex = Regex::new("a testcase")?;

        assert_eq!(regex.match_substring("Contains a testcase"),
                   ("Contains ".len(), "Contains a testcase".len()));
        assert_eq!(regex.match_substring("Some other string"),
                   (0, 0));
        Ok(())
    }
}
