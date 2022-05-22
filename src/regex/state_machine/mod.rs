use std::vec::Vec;

pub mod machine;
use machine::{StateRegister, NFA};

/// Convert a regular expression to a NFA.
///
/// Uses the Shunting-Yard algorithm to convert a regex written in
/// infix notation to a postifix regex. Then applies the Thompson
/// construction to that postfix regex in order to convert it into a
/// NFA. The two algorithms are combined so that this is done in a
/// single loop over the regex characters. We don't use explicit
/// concatenation characters in the infix notation, while the postfix
/// notation requires it. This requires some logic to decide
/// where the "would be" a concatenation character in the postifix
/// notation.
pub fn regex_to_nfa<'a>(regex: &'a str) -> Result<NFA<'a>, &'static str> {
    check_for_invalid(regex)?;

    let mut nfa_builder = NFABuilder {
        register: StateRegister::new(),
        fragment_stack: Vec::new(),
        operator_stack: Vec::new(),
    };

    let mut previous_char: Option<char> = None;
    let mut regex_char_indices = regex.char_indices();

    while let Some((char_byte_index, character)) = regex_char_indices.next() {
        // Determine if the current character should be concatenated
        // with the previous. If so we temporarily act as if we were
        // looking at a concatenation character (~). Since in infix
        // notation this operator would be between the two characters
        // this logic needs to go first.
        let mut concatenate_previous: bool = false;

        if let Some(previous_char_inner) = previous_char {
            concatenate_previous = are_concatenated(previous_char_inner, character);
        }

        if concatenate_previous {
            nfa_builder.handle_operator('~')?;
        }

        // The escape character. Treat the next character as literal no
        // matter what.
        if character == '\\' {
            if let Some((_, next_character)) = regex_char_indices.next() {
                nfa_builder.parse_literal_to_nfa(next_character);
            } else {
                return Err("Regex cannot end in a escape");
            }

            // To handle concatenation correctly on the next iteration the
            // previous character must be recongized as a literal char.
            // The actual value does not matter.
            previous_char = Some('a');
            continue;
        }

        // A bracket character class, whose NFA state stores a slice
        // into the regex.
        if character == '[' {
            let mut end_bracket_char_byte_index: Option<usize> = None;
            while let Some((i, character)) = regex_char_indices.next() {
                if character == ']' {
                    end_bracket_char_byte_index = Some(i);
                    break;
                }
            }

            if end_bracket_char_byte_index.is_none() {
                return Err("End of regex before end of bracket");
            }

            nfa_builder.parse_bracket_character_class_to_nfa(
                &regex[(char_byte_index + 1)..end_bracket_char_byte_index.unwrap()],
            );

            previous_char = Some(']');
            continue;
        }

        // Handle the current character
        match character {
            // Operators. Exluding concatenation which is not treated as
            // an operator in the infix notation so should match a literal
            // character here.
            '*' | '+' | '?' | '|' => {
                nfa_builder.handle_operator(character)?;
            }
            // Parentheses: grouping
            '(' => {
                nfa_builder.operator_stack.push('(');
            }
            ')' => {
                nfa_builder.handle_closing_paren()?;
            }
            '.' => {
                nfa_builder.parse_dot_character_class_to_nfa();
            }
            // Default: Literal character
            _ => {
                nfa_builder.parse_literal_to_nfa(character);
            }
        }

        previous_char = Some(character);
    }

    nfa_builder.empty_operator_stack()?;

    let final_nfa_fragment = nfa_builder.finalize()?;

    return Ok(NFA {
        state_register: nfa_builder.register,
        start_state: final_nfa_fragment.start,
    });
}

/// A graph of states with a single input state and any number of end
/// states with dangling (`None`) out arrows. States are represented
/// by their unique id in reference to a given StateRegister.
struct Fragment {
    start: usize,
    ends: Vec<usize>,
}

impl<'a> Fragment {
    /// Attach all the unattached (`None`) outgoing lines of all the end states
    /// of the fragment to a given state.
    fn connect_ends(&self, to_state: usize, register: &mut StateRegister<'a>) {
        for end in &self.ends[..] {
            register.connect_dangling_outs_to_state(*end, to_state);
        }
    }
}

struct NFABuilder<'a> {
    fragment_stack: Vec<Fragment>,
    operator_stack: Vec<char>,
    register: StateRegister<'a>,
}

impl<'a> NFABuilder<'a> {
    /// After parsing all regex characters there should be only one
    /// fragment left in the stack for a valid regex. Letting all the
    /// ends of that fragment point to the matching state completes
    /// the construction of the NFA.
    fn finalize(&mut self) -> Result<Fragment, &'static str> {
        let final_fragment_or_none = self.fragment_stack.pop();

        if self.fragment_stack.len() > 0 {
            // More than one fragment left after parsing all
            // characters means the passed regex was ill formed.
            // TODO: Better error message: when does this happen?
            return Err("Invalid regex. More than one final fragment in construction.");
        }

        if final_fragment_or_none.is_none() {
            // TODO: Better error message: when does this happen?
            return Err("Unexpected empty stack after loop end!");
        }

        let final_fragment = final_fragment_or_none.unwrap();
        let match_state = self.register.match_state();
        final_fragment.connect_ends(match_state, &mut self.register);

        Ok(final_fragment)
    }

    /// While iterating over the regex characters, handle a operator.
    /// Note that an operator here is not the same as a metacharacter.
    /// The former is a subset of the latter.
    fn handle_operator(&mut self, operator: char) -> Result<(), &'static str> {
        while self.should_pop_from_operator_stack(operator) {
            match self.operator_stack.pop() {
                Some(op) => self.parse_operator_to_nfa(op)?,
                None => break,
            }
        }

        self.operator_stack.push(operator);
        Ok(())
    }

    /// When a grouping is over, as signaled by a closing parentheis,
    /// pop the operator stack until we find the start of the grouping.
    fn handle_closing_paren(&mut self) -> Result<(), &'static str> {
        while let Some(operator) = self.operator_stack.pop() {
            if operator == '(' {
                return Ok(());
            }

            self.parse_operator_to_nfa(operator)?;
        }

        Err("Unmatched parenthesis: Could not find opening parenthesis.")
    }

    /// Parse all remaining operators on the stack
    fn empty_operator_stack(&mut self) -> Result<(), &'static str> {
        while let Some(operator) = self.operator_stack.pop() {
            if operator == '(' {
                // A parenthesis left in the operator stack is not
                // possible after looping over all characters
                // unless it was unmatched.
                return Err("Unmatched parentheis: Could not find closing parenthesis.");
            }
            self.parse_operator_to_nfa(operator)?;
        }
        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a regex operator
    ///
    /// Part of the `regex_to_nfa` algorithm.  Note that operator here
    /// is not the same as "metacharacter", which is a broader class.
    fn parse_operator_to_nfa(&mut self, operator: char) -> Result<(), &'static str> {
        match operator {
            // Alteration (or)
            '|' => {
                self.parse_alteration_operator_to_nfa()?;
            }
            // Concatenation (and)
            '~' => {
                self.parse_concatenation_operator_to_nfa()?;
            }
            // Zero or more
            '*' => {
                self.parse_zero_or_more_operator_to_nfa()?;
            }
            // One or more
            '+' => {
                self.parse_one_or_more_operator_to_nfa()?;
            }
            // Zero or one
            '?' => {
                self.parse_zero_or_one_operator_to_nfa()?;
            }
            _ => {
                panic!("Invalid regex operator found in operator stack.")
            }
        }

        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a literal char.
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_literal_to_nfa(&mut self, literal_char: char) {
        let literal = self.register.new_literal(literal_char, None);

        let single_literal_fragment = Fragment {
            start: literal,
            ends: vec![literal],
        };
        self.fragment_stack.push(single_literal_fragment);
    }

    /// Create a NFA fragment given `fragment_stack` and a concatenation.
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_concatenation_operator_to_nfa(&mut self) -> Result<(), &'static str> {
        if self.fragment_stack.len() < 2 {
            // Since the concatenation operator is implicit in the regex
            // notation used by the end user, a erroneous state like the
            // one encountered here is not due to the user writing a
            // malformed regex.  Instead this would be a bug with the
            // functionality that converts the implicit concatenation to
            // explicit concatation, therefore we panic here instead of
            // erroring.
            panic!("Too few arguments for concatenation operator. Two required.")
        }

        // Connect the ends of fragment_1 to the start of fragment_2
        let fragment_2 = self.fragment_stack.pop().expect("Pop of empty vector");
        let fragment_1 = self.fragment_stack.pop().expect("Pop of empty vector");

        fragment_1.connect_ends(fragment_2.start, &mut self.register);

        // Fuse the two fragments together to a single fragment,
        // and push that to the stack
        let fused_fragment = Fragment {
            start: fragment_1.start,
            ends: fragment_2.ends,
        };
        self.fragment_stack.push(fused_fragment);
        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a alteration.
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_alteration_operator_to_nfa(&mut self) -> Result<(), &'static str> {
        if self.fragment_stack.len() < 2 {
            return Err("The alteration operator requires two operands but fewer where found. Have you forgotten to escape a operator?");
        }

        let fragment_2 = self.fragment_stack.pop().expect("Pop of empty vector");
        let fragment_1 = self.fragment_stack.pop().expect("Pop of empty vector");

        // Create a new split state which has the start states of the
        // two fragments as the two choices.
        let split = self
            .register
            .new_split(Some(fragment_1.start), Some(fragment_2.start));

        // Collect this into a new fragment with the split state as
        // the start state and with the union of the ends of the two
        // fragments as the new vector of ends.
        let split_fragment = Fragment {
            start: split,
            ends: [&fragment_1.ends[..], &fragment_2.ends[..]].concat(),
        };
        self.fragment_stack.push(split_fragment);
        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a "?"
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_zero_or_one_operator_to_nfa(&mut self) -> Result<(), &'static str> {
        if self.fragment_stack.len() < 1 {
            return Err("No valid operand found for the zero or one operator. Have you forgotten to escape a operator?");
        }

        let fragment = self.fragment_stack.pop().expect("Pop of empty vector");
        let split_state = self.register.new_split(Some(fragment.start), None);

        let zero_or_one_fragment = Fragment {
            start: split_state,
            ends: [&fragment.ends[..], &[split_state]].concat(),
        };

        self.fragment_stack.push(zero_or_one_fragment);
        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a "*".
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_zero_or_more_operator_to_nfa(&mut self) -> Result<(), &'static str> {
        if self.fragment_stack.len() < 1 {
            return Err("No valid operand found for the zero or more operator. Have you forgotten to escape a operator?");
        }

        let fragment = self.fragment_stack.pop().expect("Pop of empty vector");
        let split_state = self.register.new_split(Some(fragment.start), None);

        fragment.connect_ends(split_state, &mut self.register);

        let zero_or_more_fragment = Fragment {
            start: split_state,
            ends: vec![split_state],
        };

        self.fragment_stack.push(zero_or_more_fragment);
        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a "+".
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_one_or_more_operator_to_nfa(&mut self) -> Result<(), &'static str> {
        if self.fragment_stack.len() < 1 {
            return Err("No valid operand found for the one or more operator. Have you forgotten to escape a operator?");
        }

        let fragment = self.fragment_stack.pop().expect("Pop of empty vector");
        let split_state = self.register.new_split(Some(fragment.start), None);

        fragment.connect_ends(split_state, &mut self.register);

        let one_or_more_fragment = Fragment {
            start: fragment.start,
            ends: vec![split_state],
        };

        self.fragment_stack.push(one_or_more_fragment);
        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a dot.
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_dot_character_class_to_nfa(&mut self) {
        let dot = self.register.new_dot(None);

        let single_dot_fragment = Fragment {
            start: dot,
            ends: vec![dot],
        };
        self.fragment_stack.push(single_dot_fragment);
    }

    /// Create a NFA fragment given `fragment_stack` and a bracket.
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_bracket_character_class_to_nfa<'b: 'a>(&mut self, bracketed_expression: &'b str) {
        let bracket = self.register.new_bracket(bracketed_expression, None);

        let single_bracket_fragment = Fragment {
            start: bracket,
            ends: vec![bracket],
        };
        self.fragment_stack.push(single_bracket_fragment);
    }

    /// Should we pop from the operator stack before adding the new operator?
    fn should_pop_from_operator_stack(&self, new_operator: char) -> bool {
        if self.operator_stack.len() == 0 {
            return false;
        }
        if self.operator_stack.last() == Some(&'(') {
            return false;
        }

        precedence(*self.operator_stack.last().unwrap()) > precedence(new_operator)
    }
}

/// Operator precedence for regex operators. Higher value
/// means higher precedence.
fn precedence(regex_operator: char) -> usize {
    match regex_operator {
        '|' => 1, // Alteration (or)
        '~' => 3, // Concatenation (and)
        '*' => 4, // Zero or more
        '+' => 4, // One or more
        '?' => 4, // Zero or one
        _ => {
            panic!("Invalid regex operator")
        }
    }
}

fn check_for_invalid(regex: &str) -> Result<(), &'static str> {
    if regex == "" {
        return Err("Empty regex pattern not supported");
    }
    // TODO: Add more checks.
    Ok(())
}

/// Determine if two characters are concatenated if they appear after
/// each other in a infix regex.
fn are_concatenated(character: char, next_character: char) -> bool {
    return !(['*', '+', '?', '|', ')'].contains(&next_character)
        || ['|', '('].contains(&character));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_nfa_matching_1() -> Result<(), &'static str> {
        // Test alteration, unicode, concatenation
        let regex: &str = "(a|⻘)c";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ac", false), Some(2));
        assert_eq!(nfa.simulate("⻘c", false), Some("⻘c".len()));
        assert_eq!(nfa.simulate("a", false), None); // Missing c
        assert_eq!(nfa.simulate("c", false), None); // Missing first char
        assert_eq!(nfa.simulate("xc", false), None); // Wrong first char
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_2() -> Result<(), &'static str> {
        // Test one or more, zero or more
        let regex: &str = "(a|b)*c+";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ac", false), Some(2));
        assert_eq!(nfa.simulate("c", false), Some(1));
        assert_eq!(nfa.simulate("aaaac", false), Some(5));
        assert_eq!(nfa.simulate("accccc", false), Some(2)); // 2 not greedy
        assert_eq!(nfa.simulate("bc", false), Some(2));
        assert_eq!(nfa.simulate("abc", false), Some(3)); // Both characters allowed in zero or more
        assert_eq!(nfa.simulate("b", false), None); // Too few c
        assert_eq!(nfa.simulate("", false), None); // Too few c
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_3() -> Result<(), &'static str> {
        // Test character classes
        let regex: &str = ".*a[0-9]+"; // Any string that ends in `a` + number
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a2021", true), Some(5));
        assert_eq!(nfa.simulate("åa9", true), Some("åa9".len()));
        assert_eq!(nfa.simulate("教育漢字a0", true), Some("教育漢字a0".len()));
        assert_eq!(nfa.simulate("b", true), None);
        assert_eq!(nfa.simulate("a", true), None);
        assert_eq!(nfa.simulate("aO", true), None);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_4() -> Result<(), &'static str> {
        // Test escaping
        let regex: &str = r"(\.\*)+"; // One or more literal .*
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate(".*.*.*.*", true), Some(8));
        assert_eq!(nfa.simulate(".*.*.*.*", false), Some(2));
        assert_eq!(nfa.simulate(".*", false), Some(2));
        assert_eq!(nfa.simulate(".", false), None);
        assert_eq!(nfa.simulate("a", false), None);
        assert_eq!(nfa.simulate("\\.\\*", false), None);
        assert_eq!(nfa.simulate("\\.", false), None);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_5() -> Result<(), &'static str> {
        // Test or and concatenation
        let regex: &str = "a(b|c)";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ab", false), Some(2));
        assert_eq!(nfa.simulate("ac", false), Some(2));
        assert_eq!(nfa.simulate("abx", false), Some(2));
        assert_eq!(nfa.simulate("a", false), None);
        assert_eq!(nfa.simulate("aa", false), None);
        assert_eq!(nfa.simulate("", false), None);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_6() -> Result<(), &'static str> {
        // Test zero or more, one or more, zero or one
        // Test unicode support
        let regex: &str = "a*ø?⻘+";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a⻘", false), Some("a⻘".len()));
        assert_eq!(nfa.simulate("aø⻘", false), Some("aø⻘".len()));
        assert_eq!(nfa.simulate("⻘", false), Some("⻘".len()));
        assert_eq!(nfa.simulate("ø⻘", false), Some("ø⻘".len()));
        assert_eq!(
            nfa.simulate("aaaaaaaaaaaa⻘⻘⻘⻘⻘⻘", false),
            Some("aaaaaaaaaaaa⻘".len())
        );
        assert_eq!(nfa.simulate("aøø⻘", false), None); // Too many ø
        assert_eq!(nfa.simulate("aø", false), None); // Too few ⻘
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_7() -> Result<(), &'static str> {
        // Test detection of a match on the empty string.
        let regex: &str = "a*";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a", false), Some(0));
        assert_eq!(nfa.simulate("a", true), Some(1));
        assert_eq!(nfa.simulate("", false), Some(0));
        assert_eq!(nfa.simulate("", true), Some(0));
        assert_eq!(nfa.simulate("b", false), Some(0));
        assert_eq!(nfa.simulate("b", true), Some(0));
        Ok(())
    }

    // Test that grouping without applying any operation on the group gives the same results as if
    // the group was not there.
    #[test]
    fn test_grouping_with_no_operator() -> Result<(), &'static str> {
        let regex: &str = "x(y)z";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("xy", true), None);
        assert_eq!(nfa.simulate("xz", true), None);
        assert_eq!(nfa.simulate("xyz", true), Some(3));
        Ok(())
    }

    /// Test greedy vs non-greedy matching
    #[test]
    fn test_greedyness() -> Result<(), &'static str> {
        let mut nfa = regex_to_nfa("a+")?;
        assert_eq!(nfa.simulate("aaa", true), Some(3));
        assert_eq!(nfa.simulate("aaa", false), Some(1));

        nfa = regex_to_nfa("a*")?;
        assert_eq!(nfa.simulate("aaa", true), Some(3));
        assert_eq!(nfa.simulate("aaa", false), Some(0));

        nfa = regex_to_nfa("[0-9]+")?;
        assert_eq!(nfa.simulate("2002", true), Some("2002".len()));
        assert_eq!(nfa.simulate("2002", false), Some(1));

        Ok(())
    }
}
