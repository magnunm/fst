/// Regular expressions string matching by converting the regular expression to
/// a NFA (non-deterministic finite automaton).
use std::str;

mod state_machine;
use state_machine::machine::NFA;
use state_machine::regex_to_nfa;

/// A regular expression string, and functions to match a input string to it.
pub struct Regex<'a> {
    pub regex: &'a str,

    literal_string_head: &'a str,
    literal_string_tail: &'a str,
    // The NFA constructed from the regex and used internally for matching.
    nfa: NFA<'a>,
    // Whether or not the regex started with a caret (^) and/or ends with a
    // dollar ($).
    from_start: bool,
    until_end: bool,

    is_pure_literal: bool,
}

impl<'a> Regex<'a> {
    pub fn new(regex: &str) -> Result<Regex, &'static str> {
        let starts_with_caret = regex.chars().next() == Some('^');
        let ends_with_dollar = regex.chars().rev().next() == Some('$');

        // The NFA regex matching does not implement the caret or dollar,
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
        let nfa = regex_to_nfa(stripped_regex)?;

        // Detect and record literal strings at the head and tail of
        // the regex.
        let non_literal_range = non_literal_range(stripped_regex);
        let mut is_pure_literal = false;
        if non_literal_range.0 == stripped_regex.len() {
            is_pure_literal = true;
        }

        let literal_string_head = &stripped_regex[0..non_literal_range.0];
        let literal_string_tail = &stripped_regex[non_literal_range.1..stripped_regex.len()];

        Ok(Regex {
            regex,
            literal_string_head,
            literal_string_tail,
            nfa,
            from_start: starts_with_caret,
            until_end: ends_with_dollar,
            is_pure_literal,
        })
    }

    /// Match the regex to a substring of `input`
    ///
    /// Literal string heads and tails on the regex are searched for and used to reduce the input
    /// needed to run the NFA on. The matching once a start and end position is chosen is handled
    /// by the NFA created from the truncated regex string. Returns the char byte index of the char
    /// where the matching substring starts and the first char after it.
    pub fn match_substring(&self, input: &str) -> (usize, usize) {
        let mut input_for_nfa_start: usize = 0;
        if self.has_literal_string_head() {
            let byte_index_of_first_head_literal = input.find(self.literal_string_head);

            match byte_index_of_first_head_literal {
                None => return (0, 0),
                Some(byte_index) => {
                    if self.is_pure_literal {
                        return (byte_index, byte_index + self.literal_string_head.len());
                    }
                    input_for_nfa_start = byte_index
                }
            }
        }

        let mut input_for_nfa_end: usize = input.len();
        if self.has_literal_string_tail() {
            let byte_index_of_tail_literal = input.rfind(self.literal_string_tail);

            match byte_index_of_tail_literal {
                None => return (0, 0),
                Some(byte_index) => {
                    input_for_nfa_end = byte_index + self.literal_string_tail.len();
                }
            }
        }

        if input_for_nfa_end < input_for_nfa_start {
            // Both literal tail and literal head, but there is no match on the
            // literal head after the match on the head. Then the full input can
            // not possibly match.
            return (0, 0);
        }

        let input_for_nfa = &input[input_for_nfa_start..input_for_nfa_end];
        let relative_result: (usize, usize) =
            match self.nfa_match_substring(input_for_nfa, self.from_start) {
                Some(res) => res,
                None => return (0, 0),
            };

        let result = (
            input_for_nfa_start + relative_result.0,
            input_for_nfa_start + relative_result.1,
        );

        // TODO: These checks can be done much earlier.
        if self.until_end && result.1 != input.len() {
            return (0, 0);
        }
        if self.from_start && result.0 != 0 {
            return (0, 0);
        }

        return result;
    }

    fn nfa_match_substring(&self, input: &str, from_start: bool) -> Option<(usize, usize)> {
        if from_start {
            if let Some(match_end_byte_index) = self.nfa.simulate(input, true) {
                return Some((0, match_end_byte_index));
            }
            return None;
        }

        for (byte_index, _) in input.char_indices() {
            if let Some(relative_match_end) = self.nfa.simulate(&input[byte_index..], true) {
                return Some((byte_index, byte_index + relative_match_end));
            }
        }

        return None;
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

    // NOTE: this will give a non-literal range which has the end one further than needed in the
    // case of operators. This because the operators work on the char to the left of themselves,
    // not to the right.
    let end_index_non_literal: usize =
        regex.len() - first_non_literal_regex_char(&regex.chars().rev().collect::<String>());
    return (start_index_non_literal, end_index_non_literal);
}

fn first_non_literal_regex_char(regex: &str) -> usize {
    // Finding an alteration before any grouping means any potential literal string head found this
    // far is not valid. This since it is alterated with some other expression which might not
    // contain the literal string.
    if let Some(first_alteration) = find_predicate(regex, |c| c == '|') {
        let first_grouping = find_predicate(regex, is_regex_grouping);
        if first_grouping.is_none() || first_grouping.unwrap() > first_alteration {
            return 0;
        }
    }

    let mut regex_char_indices = regex.char_indices();
    let mut index_non_literal = 0;
    let mut previous_char_size_bytes: usize = 0;

    while let Some((char_byte_index, character)) = regex_char_indices.next() {
        index_non_literal = char_byte_index;

        if not_part_of_regex_literal(character) {
            if is_regex_operator(character) {
                // These work on a single character, excluding
                // alterations which are handled separately.
                return index_non_literal - previous_char_size_bytes;
            }

            return index_non_literal;
        }

        previous_char_size_bytes = character.len_utf8();
    }

    // Done with the loop because we reached the end of the regex characters without detecting a
    // single non-literal char. Then the last char was also literal, and we need to add its length.
    return index_non_literal + previous_char_size_bytes;
}

fn not_part_of_regex_literal(c: char) -> bool {
    // TODO: Backslash can be handled better, the string is
    // still literal but the backslash should be removed
    // in the literal search
    ['\\', '|'].contains(&c)
        || is_regex_grouping(c)
        || is_regex_operator(c)
        || is_regex_character_class(c)
}

fn is_regex_operator(c: char) -> bool {
    ['+', '?', '*', '|'].contains(&c)
}

fn is_regex_character_class(c: char) -> bool {
    ['[', ']', '.'].contains(&c)
}

fn is_regex_grouping(c: char) -> bool {
    ['(', ')'].contains(&c)
}

/// Byte index of first character matching the predicate.
fn find_predicate(in_str: &str, predicate: fn(char) -> bool) -> Option<usize> {
    in_str
        .char_indices()
        .filter(|&(_, c)| predicate(c))
        .map(|(i, _)| i)
        .next()
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

        assert_eq!(
            regex.match_substring("lorem ipsum a2021"),
            (0, "lorem ipsum a2021".len())
        );
        assert_eq!(regex.match_substring("a2021"), (0, "a2021".len()));
        assert_eq!(regex.match_substring("教漢"), (0, "教漢".len()));
        assert_eq!(regex.match_substring("abc"), (0, 0));
        Ok(())
    }

    #[test]
    fn test_regex_substring_matching_with_head_literal() -> Result<(), &'static str> {
        let regex = Regex::new("Testcase[0-9]+")?;

        assert_eq!(regex.match_substring("Testcase1"), (0, "Testcase1".len()));
        assert_eq!(
            regex.match_substring("This is Testcase2"),
            ("This is ".len(), "This is Testcase2".len())
        );
        assert_eq!(
            regex.match_substring("Testcase"), // Missing the number
            (0, 0)
        );
        assert_eq!(
            regex.match_substring("12"), // Missing the literal
            (0, 0)
        );
        Ok(())
    }

    #[test]
    fn test_regex_substring_matching_with_tail_literal() -> Result<(), &'static str> {
        let regex = Regex::new("(T|t)estcase")?;

        assert_eq!(regex.match_substring("Testcase"), (0, "Testcase".len()));
        assert_eq!(
            regex.match_substring("This is a testcase"),
            ("This is a ".len(), "This is a testcase".len())
        );
        assert_eq!(
            regex.match_substring("Testcases are everywhere"),
            (0, "Testcase".len())
        );
        assert_eq!(regex.match_substring("estcase"), (0, 0));
        Ok(())
    }

    #[test]
    fn test_regex_substring_matching_pure_literal() -> Result<(), &'static str> {
        let regex = Regex::new("a testcase")?;

        assert_eq!(
            regex.match_substring("Contains a testcase"),
            ("Contains ".len(), "Contains a testcase".len())
        );
        assert_eq!(regex.match_substring("Some other string"), (0, 0));
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

        assert_eq!(regex.match_substring("Testcas教 estcase"), (0, 0));
        Ok(())
    }

    // Test handling of both a literal head and a literal tail. In particular the case where the
    // tail is found earlier in the input than the head. This means there is not a match.
    #[test]
    fn test_slicing_with_literal_head_and_tail() -> Result<(), &'static str> {
        let regex = Regex::new("fo(o)+ bar")?;

        assert_eq!(regex.match_substring(" bar foo"), (0, 0));
        Ok(())
    }

    // Test that even if the first match to the literal head does not lead to the true match we are
    // able to continue.
    #[test]
    fn test_second_match_on_literal_head_is_true_match() -> Result<(), &'static str> {
        let regex = Regex::new("cat(s)? playing")?;

        assert_eq!(regex.match_substring("cats, cats playing"), (6, 18));
        Ok(())
    }

    #[test]
    fn test_from_start() -> Result<(), &'static str> {
        let regex = Regex::new("^foo?")?;

        assert_eq!(regex.match_substring("foo"), (0, 3));
        assert_eq!(regex.match_substring("fo"), (0, 2));
        assert_eq!(regex.match_substring("  foo"), (0, 0)); // No match since from start.
        Ok(())
    }

    #[test]
    fn test_until_end() -> Result<(), &'static str> {
        let regex = Regex::new(".*a$")?;

        assert_eq!(regex.match_substring("app"), (0, 0));
        assert_eq!(regex.match_substring("a "), (0, 0)); // No match since until end
        assert_eq!(regex.match_substring("baaa"), (0, 4));
        Ok(())
    }

    #[test]
    fn test_non_literal_range() -> Result<(), &'static str> {
        assert_eq!(non_literal_range("abc?"), (2, 4)); // == "c?"
        assert_eq!(non_literal_range("abc|def"), (0, "abc|def".len()));
        assert_eq!(non_literal_range("(abc)def"), (0, "(abc)".len()));

        // NOTE: In these operator cases gives the end of the non-literal range as one to much.
        // Limitation of the implementation that has no impact on overall correctness.
        assert_eq!(non_literal_range("abc?de"), (2, 5)); // == "c?d"

        Ok(())
    }

    #[test]
    fn test_first_non_literal_char() -> Result<(), &'static str> {
        // First non-literal is `c` since it is operated on by `?`.
        assert_eq!(first_non_literal_regex_char("abc?"), 2);
        assert_eq!(first_non_literal_regex_char("abc?def"), 2);

        assert_eq!(first_non_literal_regex_char("abc"), 3);
        assert_eq!(first_non_literal_regex_char("foo|bar"), 0);
        assert_eq!(first_non_literal_regex_char("ab*|x"), 0);
        assert_eq!(first_non_literal_regex_char("ab|x+"), 0);
        assert_eq!(first_non_literal_regex_char("a(b*|x)"), 1);
        assert_eq!(first_non_literal_regex_char("a|(b*x)"), 0);

        Ok(())
    }
}
