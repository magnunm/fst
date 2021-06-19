/// Regular expressions string matching by converting the regular expression to
/// a NFA (non-deterministic finite automaton).
///
/// Works by first converting the regular expression to postfix notation and
/// then applying Thompson's construction to that expression.
/// The NFA is represented by states in a state register, to simulate the
/// NFA we need this and the id of the start state of the NFA.
use std::str;
use std::vec::Vec;
use std::fmt;
use std::collections::HashSet;
use std::mem;

/// A regular expression string, and functions to match a string to it.
pub struct Regex<'a> {
    pub regex: &'a str,

    // The NFA constructed from the regex and used internally for matching.
    nfa: NFA<'a>,
    // Wether or not the regex started with a caret (^) and/or ends with a
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
        let nfa = regex_to_nfa(nfa_regex)?;

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
fn regex_to_nfa<'a>(regex: &'a str) -> Result<NFA<'a>, &'static str> {
    let mut nfa_builder = NFABuilder {
        register: StateRegister::new(),
        fragment_stack: Vec::new(),
        operator_stack: Vec::new()
    };

    let mut previous_char: Option<char> = None;
    let mut regex_char_indices = regex.char_indices();

    loop {
        let character_and_index_or_none = regex_char_indices.next();
        if character_and_index_or_none.is_none() { break; }

        let (char_byte_index, character) = character_and_index_or_none.unwrap();

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
            }
            else {
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
                &regex[(char_byte_index + 1)..end_bracket_char_byte_index.unwrap()]
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
            },
            // Parentheses: grouping
            '(' => {
                nfa_builder.operator_stack.push('(');
            },
            ')' => {
                nfa_builder.handle_closing_paren()?;
            }
            '.' => {
                nfa_builder.parse_dot_character_class_to_nfa();
            },
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
        start_state: final_nfa_fragment.start
    });
}

/// A non-deterministic finite automaton.
/// Defined by a state register which contains all the states, and the id of
/// the entry point (start state) of the NFA.
struct NFA<'a> {
    state_register: StateRegister<'a>,
    start_state: usize
}

impl<'a> NFA<'a> {
    /// Run the NFA with a given input string.
    ///
    /// The simulation can be in multiple NFA states at the same time.
    /// Returns the character byte index of the first character after the
    /// string that matches the pattern. A return value of 0 means there
    /// was no match at all.
    /// `greedy` controls wether or not we will match the tail of the
    /// regex greedily or not. If true the returned byte index will be
    /// the first character after the longest matching substring in
    /// `input`, if `false` it will be the index after the shortest
    /// matching substring in `input`.
    fn simulate(&self, input: &str, greedy: bool) -> usize {
        let register: &StateRegister = &self.state_register;

        // Current states the NFA is in.
        // A hash set since it should not contain the same state twice.
        let mut current: HashSet<usize> = HashSet::new();
        insert_or_follow_split(
            &mut current,
            register.get_state(self.start_state),
            self.start_state,
            register
        );

        // States the NFA will be in after the current character
        let mut next: HashSet<usize> = HashSet::new();

        // Char byte index of the character after the longest matching
        // substring found this far.
        let mut largest_matching_char_index: usize = 0;

        // Follow the first out arrow of a state and insert the state
        // at the end of it into the next states.
        // The hash set guarantees this does not insert the
        // state if it is already in `next`.
        // Any split will be followed and the output
        // states of that split will be added instead.
        fn follow_first_out_arrow(state: &State,
                                  next: &mut HashSet<usize>,
                                  register: &StateRegister) {
            let next_state_id = state.out[0].unwrap();
            let next_state = register.get_state(next_state_id);

            insert_or_follow_split(
                next, next_state, next_state_id, register
            );
        };

        for (byte_index, character) in input.char_indices() {
            // Use the current states to compute the next states given the
            // character in the input string
            for state_id in current.iter() {
                let state: &State = register.get_state(*state_id);

                match &state.state_type {
                    StateType::Match => {
                        // Already in the match state.
                        // If we are matching greedily we should store the
                        // char index as the largest found yet and continue to
                        // iterate the input chars.
                        // If not matching greedily we don't need to iterate
                        // further, as this is already the shortest matching
                        // substring.
                        if greedy {
                            largest_matching_char_index = byte_index;
                        }
                        else {
                            return byte_index;
                        }
                    },
                    // Literal: follow out if current matches literal's char
                    StateType::Literal(c) => {
                        if *c == character {
                            follow_first_out_arrow(state, &mut next, register);
                        }
                    },
                    // With a dot state we follow the output arrow
                    // regardless of what the current character is
                    StateType::Dot => {
                        follow_first_out_arrow(state, &mut next, register);
                    },
                    // With a bracket state we follow the output arrow if
                    // the current character matches the bracket.
                    StateType::Bracket(bracketed) => {
                        if matches_bracket(character, bracketed) {
                            follow_first_out_arrow(state, &mut next, register);
                        }
                    },
                    StateType::Split => {
                        // We should never reach a split here. That is the job of
                        // `insert_or_follow_split` to ensure.
                        panic!("Unexpected split found in `current`!");
                    }
                }
            }

            // If `next` is empty there is no need to continue
            // iterating over the characters. If matching greedily we
            // might have encountered a match already, so we should
            // return the index of that. If not this return will
            // correctly be zero.
            if next.is_empty() {
                return largest_matching_char_index;
            }

            // Next becomes the new current, and the new next is initialized.
            mem::swap(&mut current, &mut next);
            next.clear();
        }

        // If the current states contain the match state after all characters
        // are iterated over then we have a match.
        // Matching greedily this means we match the entire string, so return
        // the byte lenght of the string. Getting here not matching greedily
        // means we only now have a match, so the output is the same.
        for state in current {
            match register.get_state(state).state_type {
                StateType::Match => { return input.len(); },
                _ => ()
            }
        }
        // If not we might still have encountered a match earlier if we were
        // matching greedily. Not matching greedily this will correctly return
        // zero.
        return largest_matching_char_index;
    }
}

/// A graph of states with a single input state and any number of end
/// states with dangling (`None`) out arrows. States are represented
/// by their unique id in reference to a given StateRegister.
struct Fragment {
    start: usize,
    ends: Vec<usize>
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
    register: StateRegister<'a>
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
        if self.operator_stack.last() == Some(&'(') {
            self.operator_stack.push(operator);
            return Ok(());
        }

        while self.should_pop_from_operator_stack(operator) {
            match self.operator_stack.pop() {
                Some(op) => self.parse_operator_to_nfa(op)?,
                None => break
            }
        }

        self.operator_stack.push(operator);
        Ok(())
    }

    /// When a grouping is over, as signaled by a closing parentheis,
    /// pop the operator stack until we find the start of the grouping.
    fn handle_closing_paren(&mut self) -> Result<(), &'static str> {
        loop {
            if self.operator_stack.len() == 0 {
                return Err("Unmatched parenthesis: Could not find opening parenthesis.");
            }

            let operator_at_top = self.operator_stack.last().unwrap();
            if *operator_at_top == '(' {
                break;
            }

            match self.operator_stack.pop() {
                Some(op) => self.parse_operator_to_nfa(op)?,
                None => break
            }
        }
        self.operator_stack.pop(); // Discard both parentheses
        Ok(())
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
            },
            // Concatenation (and)
            '~' => {
                self.parse_concatenation_operator_to_nfa()?;
            },
            // Zero or more
            '*' => {
                self.parse_zero_or_more_operator_to_nfa()?;
            },
            // One or more
            '+' => {
                self.parse_one_or_more_operator_to_nfa()?;
            },
            // Zero or one
            '?' => {
                self.parse_zero_or_one_operator_to_nfa()?;
            },
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
            ends: vec![literal]
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
        let fragment_2 = pop_or_panic(&mut self.fragment_stack, None);
        let fragment_1 = pop_or_panic(&mut self.fragment_stack, None);

        fragment_1.connect_ends(fragment_2.start, &mut self.register);

        // Fuse the two fragments together to a single fragment,
        // and push that to the stack
        let fused_fragment = Fragment {
            start: fragment_1.start,
            ends: fragment_2.ends
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

        let fragment_2 = pop_or_panic(&mut self.fragment_stack, None);
        let fragment_1 = pop_or_panic(&mut self.fragment_stack, None);

        // Create a new split state which has the start states of the
        // two fragments as the two choices.
        let split = self.register.new_split(
            Some(fragment_1.start),
            Some(fragment_2.start)
        );

        // Collect this into a new fragment with the split state as
        // the start state and with the union of the ends of the two
        // fragments as the new vector of ends.
        let split_fragment = Fragment {
            start: split,
            ends: [&fragment_1.ends[..], &fragment_2.ends[..]].concat()
        };
        self.fragment_stack.push(split_fragment);
        Ok(())
    }

    /// Create a NFA fragment given `fragment_stack` and a "?"
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_zero_or_one_operator_to_nfa (&mut self) -> Result<(), &'static str> {
        if self.fragment_stack.len() < 1 {
            return Err("No valid operand found for the zero or one operator. Have you forgotten to escape a operator?");
        }

        let fragment = pop_or_panic(&mut self.fragment_stack, None);
        let split_state = self.register.new_split(Some(fragment.start), None);

        let zero_or_one_fragment = Fragment {
            start: split_state,
            ends: [&fragment.ends[..], &[split_state]].concat()
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

        let fragment = pop_or_panic(&mut self.fragment_stack, None);
        let split_state = self.register.new_split(Some(fragment.start), None);

        fragment.connect_ends(split_state, &mut self.register);

        let zero_or_more_fragment = Fragment {
            start: split_state,
            ends: vec![split_state]
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

        let fragment = pop_or_panic(&mut self.fragment_stack, None);
        let split_state = self.register.new_split(Some(fragment.start), None);

        fragment.connect_ends(split_state, &mut self.register);

        let one_or_more_fragment = Fragment {
            start: fragment.start,
            ends: vec![split_state]
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
            ends: vec![dot]
        };
        self.fragment_stack.push(single_dot_fragment);
    }

    /// Create a NFA fragment given `fragment_stack` and a bracket.
    ///
    /// Part of the `regex_to_nfa` algorithm.
    fn parse_bracket_character_class_to_nfa<'b: 'a>(&mut self, bracketed_expression: &'b str) {
        let bracket = self.register.new_bracket(
            bracketed_expression,
            None
        );

        let single_bracket_fragment = Fragment {
            start: bracket,
            ends: vec![bracket]
        };
        self.fragment_stack.push(single_bracket_fragment);

    }

    /// Should we pop from the operator stack before adding the new operator?
    fn should_pop_from_operator_stack(&self, new_operator: char) -> bool {
        if self.operator_stack.len() == 0 {
            return false;
        }

        precedence(*self.operator_stack.last().unwrap()) > precedence(new_operator)
    }
}

/// The state register contains and owns states. The states are accessed
/// through it, and it manages the lifetime of the states.
struct StateRegister<'a> {
    states: Vec<State<'a>>,
    current_id: usize
}

impl<'a> StateRegister<'a> {
    fn new() -> StateRegister<'a> {
        StateRegister {
            states: Vec::new(),
            current_id: 0
        }
    }

    /// Get a state by id, panic if it does not exist.
    fn get_state(&self, state_id: usize) -> &State<'a> {
        let ref_to_state_or_none = self.states.get(state_id);

        if ref_to_state_or_none.is_some() {
            return ref_to_state_or_none.unwrap()
        }
        panic!("No state with id {}", state_id);
    }

    /// Get a mutable state by id, panic if it does not exist.
    fn get_mut_state(&mut self, state_id: usize) -> &mut State<'a> {
        let mut_ref_to_state_or_none = self.states.get_mut(state_id);

        if mut_ref_to_state_or_none.is_some() {
            return mut_ref_to_state_or_none.unwrap()
        }
        panic!("No state with id {}", state_id);
    }

    /// Connect all the unconnected (`None`) out
    /// connections of a state to some state.
    fn connect_dangling_outs_to_state(&mut self, state_id: usize, to_state: usize) {
        let mut state = self.get_mut_state(state_id);

        state.out = state.out.iter().map(
            |val| {
                if val.is_some() {
                    return *val;
                }
                return Some(to_state);
            }
        ).collect();
    }

    /// Register a new state.
    /// Return the unique id of that state.
    fn new_state(&mut self, state_type: StateType<'a>, out: Vec<Option<usize>>) -> usize {
        self.states.push(State { state_type, out });

        // Increment the current id so that the returned id from this function
        // always corresponds to the postion of the state in `self.states`.
        self.current_id += 1;
        self.current_id - 1
    }

    fn new_literal(&mut self, contains: char, out_state: Option<usize>) -> usize {
        self.new_state(
            StateType::Literal(contains),
            vec![out_state]
        )
    }

    fn new_dot(&mut self, out_state: Option<usize>) -> usize {
        self.new_state(StateType::Dot, vec![out_state])
    }

    fn new_bracket(&mut self, contains: &'a str, out_state: Option<usize>) -> usize {
        self.new_state(
            StateType::Bracket(contains),
            vec![out_state]
        )
    }

    fn new_split(&mut self, out_state_1: Option<usize>, out_state_2: Option<usize>) -> usize {
        self.new_state(
            StateType::Split,
            vec![out_state_1, out_state_2]
        )
    }

    fn match_state(&mut self) -> usize {
        self.new_state(
            StateType::Match,
            vec![]
        )
    }
}

/// A state in a NFA (non-deterministic finite automaton).
struct State<'a> {
    state_type: StateType<'a>,

    // The states at the ends of the outgoing arrows of this state, if any
    // These states are referenced by their id, to find the actual states
    // one must go via the StateRegister.
    out: Vec<Option<usize>>
}

impl<'a> fmt::Display for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.state_type {
            StateType::Literal(c) => write!(f, "Literal ({}) ->", c),
            StateType::Dot => write!(f, "Dot (.) ->"),
            StateType::Bracket(bracketed) => write!(f, "Bracket ([{}]) ->", &bracketed),
            StateType::Split => write!(f, "Split <- () ->"),
            StateType::Match => write!(f, "MATCH ()")
        }
    }
}

/// Types of states in the NFA's built from regular expressions.
enum StateType<'a> {
    // Connects to two states via empty/epsilon arrows
    Split,
    // The special state representing a match to the regex. No outgoing arrows.
    Match,
    // A literal character. Contains just a single arrow out with that character.
    Literal(char),
    // The dot character class. Represents any single character
    Dot,
    // A bracket character class. Represents any character in the bracket
    // expression. Stores the bracket expression as a slice into the regex.
    Bracket(&'a str),
}

/// Operator precedence for regex operators. Higher value
/// means higher precedence.
fn precedence(regex_operator: char) -> usize {
    match regex_operator {
        '|' => 1,  // Alteration (or)
        '~' => 3,  // Concatenation (and)
        '*' => 4,  // Zero or more
        '+' => 4,  // One or more
        '?' => 4,  // Zero or one
        _ => { panic!("Invalid regex operator") }
    }
}

/// Insert the state id of a state into `into`, unless the state is a split.
/// For a split follow the out arrows and call this function recursively on
/// the states they point to.
fn insert_or_follow_split(into: &mut HashSet<usize>, state: &State, state_id: usize, register: &StateRegister) {
    if let StateType::Split = state.state_type {
        // `flat_map` to filter out `None` arrows
        for state_out_id in state.out.iter().flat_map(|id| *id) {
            let state_out: &State = register.get_state(state_out_id);

            insert_or_follow_split(
                into, state_out, state_out_id, register
            );
        }
    }
    else {
        into.insert(state_id);
    }
}

/// Traverse a NFA given by a start state and state register to which it
/// belongs, printing the nodes as we go along.
/// Only used for debugging the creation of the NFA.
fn print_nfa(start_id: usize, register: &StateRegister, visited: &mut HashSet<usize>) {
    let start = register.get_state(start_id);

    // To avoid infinte recursion we need to remember the states we
    // have already visited.
    if visited.contains(&start_id) {
        println!("{}", start);
        return;
    }

    visited.insert(start_id);

    if let StateType::Match = start.state_type {
        println!("{}", start);
    }
    else {
        println!("{}", start);
        for out_state_id in &start.out[..] {
            if out_state_id.is_some() {
                print_nfa(out_state_id.unwrap(), register, visited);
            }
        }
    }
}

/// pop the last element from a vector, panic if empty
fn pop_or_panic<T>(vector: &mut Vec<T>, panic_message: Option<&'static str>) -> T {
    let result: Option<T> = vector.pop();
    if result.is_some() {
        return result.unwrap();
    }
    if let Some(message) = panic_message {
        panic!(message);
    }
    panic!("Attempted pop of empty vector.");
}

/// Check if a character mathces a regex bracket expression.
fn matches_bracket(character: char, bracketed_expression: &str) -> bool {
    let mut bracketed_chars = bracketed_expression.chars();

    // Iterate in triplets in order to support ranges.
    let mut previous: Option<char> = bracketed_chars.next();
    let mut current: Option<char> = bracketed_chars.next();
    let mut next: Option<char> = bracketed_chars.next();

    // If the first character is a caret the bracket is negated, i.e.
    // a character matches if it is not in the bracket.
    let is_negated = previous == Some('^');

    if is_negated {
        previous = current;
        current = next;
        next = bracketed_chars.next();
    }

    let mut result = false;

    while previous.is_some() {
        if current == Some('-') && next.is_some() {
            // We are looking at a valid range
            if char_in_range(character, previous.unwrap(), next.unwrap()) {
                result = true;
                break;
            }

            // Go to the next triplet of characters
            previous = bracketed_chars.next();
            current = bracketed_chars.next();
            next = bracketed_chars.next();
        }
        else {
            // Base case: three characters that are not a valid range
            if character == previous.unwrap() {
                result = true;
                break;
            }

            previous = current;
            current = next;
            next = bracketed_chars.next();
        }
    }

    if is_negated {
        return !result;
    }
    result
}

/// Is a character in the given character range.
fn char_in_range(character: char, range_start: char, range_end: char) -> bool {
    if range_start < range_end {
        return range_start <= character && character <= range_end
    }
    panic!(format!("Invalid range: {}-{}", range_start, range_end))
}

/// Determine if two characters are concatenated if they appear after
/// each other in a infix regex.
fn are_concatenated(character: char, next_character: char) -> bool {
    return !(
        ['*', '+', '?', '|', ')'].contains(&next_character) ||
            ['|', '('].contains(&character)
    );
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


    #[test]
    fn test_regex_nfa_matching_1() -> Result<(), &'static str> {
        // Test alteration, unicode, concatenation
        let regex: &str = "(a|⻘)c";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ac", false), 2);
        assert_eq!(nfa.simulate("⻘c", false), "⻘c".len());
        assert_eq!(nfa.simulate("a", false), 0);  // Missing c
        assert_eq!(nfa.simulate("c", false), 0);  // Missing first char
        assert_eq!(nfa.simulate("xc", false), 0);  // Wrong first char
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_2() -> Result<(), &'static str> {
        // Test one or more, zero or more
        let regex: &str = "(a|b)*c+";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ac", false), 2);
        assert_eq!(nfa.simulate("c", false), 1);
        assert_eq!(nfa.simulate("aaaac", false), 5);
        assert_eq!(nfa.simulate("accccc", false), 2);  // 2 not greedy
        assert_eq!(nfa.simulate("bc", false), 2);
        assert_eq!(nfa.simulate("abc", false), 3);  // Both characters allowed in zero or more
        assert_eq!(nfa.simulate("b", false), 0);  // Too few c
        assert_eq!(nfa.simulate("", false), 0);  // Too few c
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_3() -> Result<(), &'static str> {
        // Test character classes
        let regex: &str = ".*a[0-9]+";  // Any string that ends in `a` + number
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a2021", true), 5);
        assert_eq!(nfa.simulate("åa9", true), "åa9".len());
        assert_eq!(nfa.simulate("教育漢字a0", true), "教育漢字a0".len());
        assert_eq!(nfa.simulate("b", true), 0);
        assert_eq!(nfa.simulate("a", true), 0);
        assert_eq!(nfa.simulate("aO", true), 0);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_4() -> Result<(), &'static str> {
        // Test escaping
        let regex: &str = "(\\.\\*)+";  // One or more literal .*
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate(".*.*.*.*", true), 8);
        assert_eq!(nfa.simulate(".*.*.*.*", false), 2);
        assert_eq!(nfa.simulate(".*", false), 2);
        assert_eq!(nfa.simulate(".", false), 0);
        assert_eq!(nfa.simulate("a", false), 0);
        assert_eq!(nfa.simulate("\\.\\*", false), 0);
        assert_eq!(nfa.simulate("\\.", false), 0);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_5() -> Result<(), &'static str> {
        // Test or and concatenation
        let regex: &str = "a(b|c)";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ab", false), 2);
        assert_eq!(nfa.simulate("ac", false), 2);
        assert_eq!(nfa.simulate("abx", false), 2);
        assert_eq!(nfa.simulate("a", false), 0);
        assert_eq!(nfa.simulate("aa", false), 0);
        assert_eq!(nfa.simulate("", false), 0);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_6() -> Result<(), &'static str> {
        // Test zero or more, one or more, zero or one
        // Test unicode support
        let regex: &str = "a*ø?⻘+";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a⻘", false), "a⻘".len());
        assert_eq!(nfa.simulate("aø⻘", false), "aø⻘".len());
        assert_eq!(nfa.simulate("⻘", false), "⻘".len());
        assert_eq!(nfa.simulate("ø⻘", false), "ø⻘".len());
        assert_eq!(nfa.simulate("aaaaaaaaaaaa⻘⻘⻘⻘⻘⻘", false), "aaaaaaaaaaaa⻘".len());
        assert_eq!(nfa.simulate("aøø⻘", false), 0);  // Too many ø
        assert_eq!(nfa.simulate("aø", false), 0);  // Too few ⻘
        Ok(())
    }
}
