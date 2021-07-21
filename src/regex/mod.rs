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

    // The NFA constructed from the regex and used internally for matching.
    nfa: nfa::NFA<'a>,
    // Whether or not the regex started with a caret (^) and/or ends with a
    // dollar ($).
    from_start: bool,
    until_end: bool,
    // Greedy matching on the tail
    greedy: bool,
}

impl<'a> Regex<'a> {
    /// Create a new regex object given the regex string.
    ///
    /// This will construct the NFA needed to do the matching against the
    /// regex string.
    pub fn new(regex: &str, greedy: bool) -> Result<Regex, &'static str> {
        let starts_with_caret = regex.chars().next() == Some('^');
        let ends_with_dollar = regex.chars().rev().next() == Some('$');

        // The NFA regex matching does not implemet the caret or dollar,
        // so they are removed from the postifx regex used to construct the NFA.
        let mut start_index_nfa_regex: usize = 0;
        let mut end_index_nfa_regex: usize = regex.len();
        if starts_with_caret {
            start_index_nfa_regex = 1;
        }
        if ends_with_dollar {
            end_index_nfa_regex -= 1;
        }

        let nfa_regex = &regex[start_index_nfa_regex..end_index_nfa_regex];
        let nfa = nfa::regex_to_nfa(nfa_regex)?;

        Ok(Regex {
            regex,
            nfa,
            from_start: starts_with_caret,
            until_end: ends_with_dollar,
            greedy
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
        for (byte_index, _) in input.char_indices() {
            let input_substring = &input[byte_index..];

            // Since we are considering the input is a single line
            // we should when matching until the end (when the regex ends in
            // `$`) match greedily on the tail of the match. This because the
            // `$` here always means the end of the `input` string.
            let byte_index_for_match_end = self.nfa.simulate(
                input_substring,
                self.greedy || self.until_end
            );

            if byte_index_for_match_end > 0 {
                // Guaranteed match unless regex should match until end,
                // in which case we need to check that the end has been
                // reached.
                if !self.until_end || byte_index_for_match_end == input_substring.len() {
                    return (byte_index, byte_index + byte_index_for_match_end);
                }
            }

            if byte_index == 0 && self.from_start {
                // If we should only match from the start we should not
                // iterate over all the match start positions.
                return (0, 0);
            }
        }

        return (0, 0);
    }
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_substring_matching() -> Result<(), &'static str> {
        // Any string that ends in `a` + number or is any ordered selection
        // from the characters 教育漢字 but nothing more.
        let regex = Regex::new("^.*a[0-9]+|教?育?漢?字?$", true)?;

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
}
