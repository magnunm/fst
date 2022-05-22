use super::state_machine::machine::NFA;

/// A matcher is some strategy for matching a given regex to a given input string. Different
/// matchers perform well in different situations, and which matcher to use can be decided based on
/// the regex.
pub trait Matcher {
    /// Given an input line, return the byte index boundary of the first matching substring.
    fn match_substring(&self, input: &str) -> (usize, usize);
}

/// Pure state-machine based matching, with no literal optimizations.
pub struct NFAMatcher<'a> {
    pub nfa: NFA<'a>,
    // Whether or not the regex started with a caret (^) and/or ends with a
    // dollar ($).
    pub from_start: bool,
    pub until_end: bool,
}

impl<'a> Matcher for NFAMatcher<'a> {
    fn match_substring(&self, input: &str) -> (usize, usize) {
        match nfa_match_substring(&self.nfa, input, self.from_start) {
            None => (0, 0),
            Some(result) => {
                if self.until_end && result.1 != input.len() {
                    return (0, 0);
                }

                result
            }
        }
    }
}

/// Pure literal string finder. Meaning the regex contained no meta-characters.
pub struct LiteralMatcher<'a> {
    pub to_find: &'a str,
    pub from_start: bool,
    pub until_end: bool,
}

impl<'a> Matcher for LiteralMatcher<'a> {
    fn match_substring(&self, input: &str) -> (usize, usize) {
        if input.len() < self.to_find.len() {
            return (0, 0);
        }

        if self.from_start {
            if self.until_end {
                if input == self.to_find {
                    return (0, self.to_find.len());
                }
                return (0, 0);
            }

            if &input[..self.to_find.len()] == self.to_find {
                return (0, self.to_find.len());
            }
            return (0, 0);
        }
        if self.until_end {
            if &input[(input.len() - self.to_find.len())..] == self.to_find {
                return (input.len() - self.to_find.len(), input.len());
            }
            return (0, 0);
        }

        match input.find(self.to_find) {
            Some(byte_index) => (byte_index, byte_index + self.to_find.len()),
            None => (0, 0),
        }
    }
}

/// Matching by first searching for a literal head, then using the state machine to match from
/// there.
pub struct LiteralHeadMatcher<'a> {
    pub literal_head: &'a str,
    pub nfa: NFA<'a>,
    pub from_start: bool,
    pub until_end: bool,
}

impl<'a> Matcher for LiteralHeadMatcher<'a> {
    fn match_substring(&self, input: &str) -> (usize, usize) {
        let mut input_for_nfa_start: usize = 0;
        if self.from_start {
            if &input[..self.literal_head.len()] != self.literal_head {
                return (0, 0);
            }
        } else {
            match input.find(self.literal_head) {
                None => return (0, 0),
                Some(byte_index) => input_for_nfa_start = byte_index,
            }
        }

        let input_for_nfa = &input[input_for_nfa_start..];
        let relative_result: (usize, usize) =
            match nfa_match_substring(&self.nfa, input_for_nfa, self.from_start) {
                Some(res) => res,
                None => return (0, 0),
            };

        let result = (
            input_for_nfa_start + relative_result.0,
            input_for_nfa_start + relative_result.1,
        );

        if self.until_end && result.1 != input.len() {
            return (0, 0);
        }
        // No check of `from_start` here since if that is true then `result.0` is guaranteed to be
        // 0 by the time we get here.

        result
    }
}

/// Matching by first searching for a literal tail from the end of the input, then limiting the
/// state machine search to only search until that.
pub struct LiteralTailMatcher<'a> {
    pub literal_tail: &'a str,
    pub nfa: NFA<'a>,
    pub from_start: bool,
    pub until_end: bool,
}

impl<'a> Matcher for LiteralTailMatcher<'a> {
    fn match_substring(&self, input: &str) -> (usize, usize) {
        let input_for_nfa_end: usize = match input.rfind(self.literal_tail) {
            None => return (0, 0),
            Some(byte_index) => byte_index + self.literal_tail.len(),
        };

        let input_for_nfa = &input[0..input_for_nfa_end];
        let result: (usize, usize) =
            match nfa_match_substring(&self.nfa, input_for_nfa, self.from_start) {
                Some(res) => res,
                None => return (0, 0),
            };

        if self.until_end && result.1 != input.len() {
            return (0, 0);
        }
        // No check of `from_start` here since if that is true then `result.0` is guaranteed to be
        // 0 by the time we get here.

        result
    }
}

/// Combination of the literal head and literal tail matchers. For regex snadwitched between two
/// literal string search patterns. Uses those to limit the range for the state machine search in
/// both directions.
pub struct LiteralSandwitchMatcher<'a> {
    pub literal_tail: &'a str,
    pub literal_head: &'a str,
    pub nfa: NFA<'a>,
    pub from_start: bool,
    pub until_end: bool,
}

impl<'a> Matcher for LiteralSandwitchMatcher<'a> {
    fn match_substring(&self, input: &str) -> (usize, usize) {
        let mut input_for_nfa_start: usize = 0;
        if self.from_start {
            if &input[..self.literal_head.len()] != self.literal_head {
                return (0, 0);
            }
        } else {
            match input.find(self.literal_head) {
                None => return (0, 0),
                Some(byte_index) => input_for_nfa_start = byte_index,
            }
        }

        let input_for_nfa_end: usize = match input.rfind(self.literal_tail) {
            None => return (0, 0),
            Some(byte_index) => byte_index + self.literal_tail.len(),
        };

        if input_for_nfa_end < input_for_nfa_start {
            // Both literal tail and literal head found, but there is no match on the literal head
            // after the match on the head. Then the input can not possibly match.
            return (0, 0);
        }

        let input_for_nfa = &input[input_for_nfa_start..input_for_nfa_end];
        let relative_result: (usize, usize) =
            match nfa_match_substring(&self.nfa, input_for_nfa, self.from_start) {
                Some(res) => res,
                None => return (0, 0),
            };

        let result = (
            input_for_nfa_start + relative_result.0,
            input_for_nfa_start + relative_result.1,
        );

        if self.until_end && result.1 != input.len() {
            return (0, 0);
        }
        // No check of `from_start` here since if that is true then `result.0` is guaranteed to be
        // 0 by the time we get here.

        result
    }
}

#[inline]
fn nfa_match_substring(nfa: &NFA, input: &str, from_start: bool) -> Option<(usize, usize)> {
    if from_start {
        if let Some(match_end_byte_index) = nfa.simulate(input, true) {
            return Some((0, match_end_byte_index));
        }
        return None;
    }

    for (byte_index, _) in input.char_indices() {
        if let Some(relative_match_end) = nfa.simulate(&input[byte_index..], true) {
            return Some((byte_index, byte_index + relative_match_end));
        }
    }

    None
}
