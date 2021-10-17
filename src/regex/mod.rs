/// Regular expressions string matching by converting the regular expression to
/// a NFA (non-deterministic finite automaton).
use std::str;

mod nfa;

/// A regular expression string, and functions to match a input string to it.
pub struct Regex<'a> {
    pub regex: &'a str,

    literal_string_head: &'a str,
    literal_string_tail: &'a str,
    // The NFA constructed from the regex and used internally for matching.
    nfa: Option<nfa::NFA<'a>>,
    // Whether or not the regex started with a caret (^) and/or ends with a
    // dollar ($).
    from_start: bool,
    until_end: bool,
}

impl<'a> Regex<'a> {
    pub fn new(regex: &str) -> Result<Regex, &'static str> {
        let starts_with_caret = regex.chars().next() == Some('^');
        let ends_with_dollar = regex.chars().rev().next() == Some('$');

        // The NFA regex matching does not implemet the caret or dollar,
        // so they are removed from the regex string used internally.
        let mut start_index_stripped_regex: usize = 0;
        let mut end_index_stripped_regex: usize = regex.len();
        if starts_with_caret {
            start_index_stripped_regex = '^'.len_utf8();
        }
        if ends_with_dollar {
            end_index_stripped_regex -= '$'.len_utf8();
        }

        let stripped_regex = &regex[start_index_stripped_regex..end_index_stripped_regex];

        // Detect and record literal strings at the head and tail of
        // the regex.
        let non_literal_range = non_literal_range(stripped_regex);
        let nfa_regex = &stripped_regex[non_literal_range.0..non_literal_range.1];
        let nfa = if nfa_regex == "" {
            None
        } else {
            Some(nfa::regex_to_nfa(nfa_regex)?)
        };

        let literal_string_head = &stripped_regex[0..non_literal_range.0];
        let literal_string_tail = &stripped_regex[non_literal_range.1..stripped_regex.len()];

        Ok(Regex {
            regex,
            literal_string_head,
            literal_string_tail,
            nfa,
            from_start: starts_with_caret,
            until_end: ends_with_dollar
        })
    }

    /// Match the regex to a substring of `input`
    ///
    /// If caret is set it will only match a substring starting from
    /// the beginning of the input. If not it will check all possible
    /// start positions as a possible match start. If dollar is set it
    /// will require that the end of the substring match is the end of
    /// the input. Literal string heads and tails on the regex are
    /// searched for and used to reduce the input needed to run the
    /// NFA on. The matching once a start and end position is chosen
    /// is handled by the NFA created from the truncated regex
    /// string. Returns the char byte index of the char where the
    /// matching substring starts and the first char after it.
    pub fn match_substring(&self, input: &str) -> (usize, usize) {
        let mut input_for_nfa_start: usize = 0;
        if self.has_literal_string_head() {
            let byte_index_of_head_literal = input.find(self.literal_string_head);

            if byte_index_of_head_literal.is_none() {
                return (0, 0);
            }
            input_for_nfa_start = byte_index_of_head_literal.unwrap() +
                self.literal_string_head.len();
        }

        let mut input_for_nfa_end: usize = input.len();
        if self.has_literal_string_tail() {
            let byte_index_of_tail_literal = input.rfind(self.literal_string_tail);

            if byte_index_of_tail_literal.is_none() {
                return (0, 0);
            }
            input_for_nfa_end = byte_index_of_tail_literal.unwrap();
        }

        if let Some(mut result) = self.match_substring_using_nfa_and_tail(input,
                                                                          input_for_nfa_start,
                                                                          input_for_nfa_end) {
            if self.until_end && result.1 != input.len() {
                return (0, 0);
            }

            // Literal head is part of the match
            result.0 -= self.literal_string_head.len();
            return result;
        }
        (0, 0)
    }

    fn match_substring_using_nfa_and_tail(&self,
                                          input: &str,
                                          mut input_for_nfa_start: usize,
                                          input_for_nfa_end: usize) -> Option<(usize, usize)> {
        let mut relative_result;
        loop {
            let input_for_nfa = &input[input_for_nfa_start..input_for_nfa_end];

            let relative_result_or_none =
                self.nfa_match_substring(input_for_nfa,
                                         self.from_start || self.has_literal_string_head());
            if relative_result_or_none.is_none() {  // No match!
                return None;
            }
            relative_result = relative_result_or_none.unwrap();

            if !self.has_literal_string_tail() {  // No more checks needed
                break;
            }

            let index_after_nfa_match = input_for_nfa_start + relative_result.1;
            if self.tail_lenght_substring_after(input, index_after_nfa_match) == Some(self.literal_string_tail) {
                break;
            }

            // The first sub-string after the match is not the tail
            // literal, so there is no match. There could still be a
            // match further along the input though, if we haven't
            // started from a literal head.
            if self.has_literal_string_head() {
                return None;
            }
            input_for_nfa_start += first_char_len_utf8(input_for_nfa); // TODO: UTF-16
        }

        let mut result = (relative_result.0 + input_for_nfa_start,
                          relative_result.1 + input_for_nfa_start);
        // Include matched literal tail in the result range (does nothing if no tail)
        result.1 += self.literal_string_tail.len();
        return Some(result);
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
        if let Some(nfa_inner) = &self.nfa {
            return nfa_inner.simulate(
                input,
                true
            );
        }
        Some(0) // The empty regex matches the empty string
    }

    fn tail_lenght_substring_after(&self, input: &'a str, index: usize) -> Option<&'a str> {
        // Assumes &input[index] is at a char boundary
        if input.is_char_boundary(index + self.literal_string_tail.len()) {
            return Some(&input[index..(index + self.literal_string_tail.len())]);
        }
        None
    }

    fn has_literal_string_tail(&self) -> bool {
        self.literal_string_tail != ""
    }

    fn has_literal_string_head(&self) -> bool {
        self.literal_string_head != ""
    }
}

/// The smallest range of byte indices such that outside the range
/// are purely literal strings.
fn non_literal_range(regex: &str) -> (usize, usize) {
    let start_index_non_literal = first_non_literal_regex_char(regex);

    if start_index_non_literal == regex.len() {
        return (start_index_non_literal, start_index_non_literal);
    }

    let end_index_non_literal: usize = regex.len() -
        first_non_literal_regex_char(&regex
                                     .chars()
                                     .rev()
                                     .collect::<String>());
    return (start_index_non_literal, end_index_non_literal)
}

fn first_non_literal_regex_char(regex: &str) -> usize {
    let mut regex_char_indices = regex.char_indices();
    let mut index_non_literal = 0;
    let mut previous_char_size_bytes: usize = 0;

    while let Some((char_byte_index, character)) = regex_char_indices.next() {
        index_non_literal = char_byte_index;

        if not_part_of_regex_literal(character) {
            if is_regex_operator(character) {
                // These work on a single character, excluding
                // alterations which are handled separately.
                index_non_literal -= previous_char_size_bytes;
            }
            if character == '|' {
                // Finding the alteration before any gouping means any
                // potential literal string head found this far is not
                // valid. This since it is alterated with some other
                // expression which might not contain the literal
                // string.
                index_non_literal = 0;
            }

            break;
        }

        previous_char_size_bytes = character.len_utf8();  // TODO: UTF-16
    }

    if regex_char_indices.next().is_none() {
        index_non_literal += previous_char_size_bytes;
    }

    return index_non_literal;
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

fn first_char_len_utf8(s: &str) -> usize {
    if let Some(character) = s.chars().next() {
        return character.len_utf8();
    }
    0
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
    fn test_regex_substring_matching_with_tail_literal() -> Result<(), &'static str> {
        let regex = Regex::new("(T|t)estcase")?;

        assert_eq!(regex.match_substring("Testcase"),
                   (0, "Testcase".len()));
        assert_eq!(regex.match_substring("This is a testcase"),
                   ("This is a ".len(), "This is a testcase".len()));
        assert_eq!(regex.match_substring("Testcases are everywhere"),
                   (0, "Testcase".len()));
        assert_eq!(regex.match_substring("estcase"),
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

    // What happens in this check is that the `match_substring`
    // finds the final `estcase` and sets that as the end of
    // matching. Then it sees the `T` matches the nfa and goes on
    // to check if the len_bytes(estcase) next bytes matches
    // `estcase`. len_bytes(estcase) puts us not at a code point
    // boundary. That is no reason for the code to panic however,
    // it simply means there is no match.
    #[test]
    fn test_tail_literal_spawns_check_not_at_char_boundary() -> Result<(), &'static str> {
        let regex = Regex::new("(T|t)estcase")?;

        assert_eq!(regex.match_substring("Testcas教 estcase"),
                   (0, 0));
        Ok(())
    }
}
