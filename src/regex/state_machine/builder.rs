use super::machine::StateRegister;

/// A graph of states with a single input state and any number of end
/// states with dangling (`None`) out arrows. States are represented
/// by their unique id in reference to a given StateRegister.
pub struct Fragment {
    pub start: usize,
    pub ends: Vec<usize>,
}

impl<'a> Fragment {
    /// Attach all the unattached (`None`) outgoing lines of all the end states
    /// of the fragment to a given state.
    pub fn connect_ends(&self, to_state: usize, register: &mut StateRegister<'a>) {
        for end in &self.ends[..] {
            register.connect_dangling_outs_to_state(*end, to_state);
        }
    }
}

pub struct NFABuilder<'a> {
    pub fragment_stack: Vec<Fragment>,
    pub operator_stack: Vec<char>,
    pub register: StateRegister<'a>,
}

impl<'a> NFABuilder<'a> {
    /// After parsing all regex characters there should be only one
    /// fragment left in the stack for a valid regex. Letting all the
    /// ends of that fragment point to the matching state completes
    /// the construction of the NFA.
    pub fn finalize(&mut self) -> Result<Fragment, &'static str> {
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
    pub fn handle_operator(&mut self, operator: char) -> Result<(), &'static str> {
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
    pub fn handle_closing_paren(&mut self) -> Result<(), &'static str> {
        while let Some(operator) = self.operator_stack.pop() {
            if operator == '(' {
                return Ok(());
            }

            self.parse_operator_to_nfa(operator)?;
        }

        Err("Unmatched parenthesis: Could not find opening parenthesis.")
    }

    /// Parse all remaining operators on the stack
    pub fn empty_operator_stack(&mut self) -> Result<(), &'static str> {
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
    pub fn parse_operator_to_nfa(&mut self, operator: char) -> Result<(), &'static str> {
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
    pub fn parse_literal_to_nfa(&mut self, literal_char: char) {
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
    pub fn parse_concatenation_operator_to_nfa(&mut self) -> Result<(), &'static str> {
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
    pub fn parse_alteration_operator_to_nfa(&mut self) -> Result<(), &'static str> {
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
    pub fn parse_zero_or_one_operator_to_nfa(&mut self) -> Result<(), &'static str> {
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
    pub fn parse_zero_or_more_operator_to_nfa(&mut self) -> Result<(), &'static str> {
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
    pub fn parse_one_or_more_operator_to_nfa(&mut self) -> Result<(), &'static str> {
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
    pub fn parse_dot_character_class_to_nfa(&mut self) {
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
    pub fn parse_bracket_character_class_to_nfa<'b: 'a>(&mut self, bracketed_expression: &'b str) {
        let bracket = self.register.new_bracket(bracketed_expression, None);

        let single_bracket_fragment = Fragment {
            start: bracket,
            ends: vec![bracket],
        };
        self.fragment_stack.push(single_bracket_fragment);
    }

    /// Should we pop from the operator stack before adding the new operator?
    pub fn should_pop_from_operator_stack(&self, new_operator: char) -> bool {
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
