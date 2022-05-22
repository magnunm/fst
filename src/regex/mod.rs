/// Regular expressions string matching by compiling the regular expression to a state machine.
use std::str;

mod state_machine;
use state_machine::regex_to_nfa;

mod matcher;
use matcher::{
    LiteralHeadMatcher, LiteralMatcher, LiteralSandwitchMatcher, LiteralTailMatcher, Matcher,
    NFAMatcher,
};

mod utils;
use utils::non_literal_range;

/// A regular expression string, and functions to match a input string to it.
pub struct Regex<'a> {
    pub regex: &'a str,

    matcher: Box<dyn Matcher + 'a>,
}

impl<'a> Regex<'a> {
    pub fn new(regex: &'a str) -> Result<Regex, &'static str> {
        let starts_with_caret = regex.chars().next() == Some('^');
        let ends_with_dollar = regex.chars().rev().next() == Some('$');

        let stripped_regex = Regex::strip_regex(regex, starts_with_caret, ends_with_dollar);
        let matcher = Regex::choose_matcher(stripped_regex, starts_with_caret, ends_with_dollar)?;
        Ok(Regex { regex, matcher })
    }

    /// Match the regex to a substring of `input`
    pub fn match_substring(&self, input: &str) -> (usize, usize) {
        self.matcher.match_substring(input)
    }

    /// Choose which matcher to use for the given regex.
    fn choose_matcher(
        regex: &'a str,
        from_start: bool,
        until_end: bool,
    ) -> Result<Box<dyn Matcher + 'a>, &'static str> {
        // Detect and record literal strings at the head and tail of
        // the regex.
        let non_literal_range = non_literal_range(regex);

        if non_literal_range.0 == regex.len() {
            // The regex contains no meta-characters! We don't need to compile to or run the state
            // machine at all.
            return Ok(Box::new(LiteralMatcher {
                to_find: regex,
                from_start,
                until_end,
            }));
        }

        let nfa = regex_to_nfa(regex)?;
        let literal_string_head = &regex[0..non_literal_range.0];
        let literal_string_tail = &regex[non_literal_range.1..regex.len()];

        if literal_string_head.len() > 0 && literal_string_tail.len() > 0 {
            return Ok(Box::new(LiteralSandwitchMatcher {
                nfa,
                literal_tail: literal_string_tail,
                literal_head: literal_string_head,
                from_start,
                until_end,
            }));
        }
        if literal_string_head.len() > 0 {
            return Ok(Box::new(LiteralHeadMatcher {
                nfa,
                literal_head: literal_string_head,
                from_start,
                until_end,
            }));
        }
        if literal_string_tail.len() > 0 {
            return Ok(Box::new(LiteralTailMatcher {
                nfa,
                literal_tail: literal_string_tail,
                from_start,
                until_end,
            }));
        }

        Ok(Box::new(NFAMatcher {
            nfa,
            from_start,
            until_end,
        }))
    }

    /// The matcher does not implement the caret or dollar, so they must be removed from the regex
    /// string used internally by the matcher.
    fn strip_regex(regex: &'a str, starts_with_caret: bool, ends_with_dollar: bool) -> &'a str {
        let start_index_stripped_regex: usize = if starts_with_caret { '^'.len_utf8() } else { 0 };
        let end_index_stripped_regex: usize = if ends_with_dollar {
            regex.len() - '$'.len_utf8()
        } else {
            regex.len()
        };

        return &regex[start_index_stripped_regex..end_index_stripped_regex];
    }
}

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
}
