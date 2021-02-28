use std::str;
use std::string::String;
use std::vec::Vec;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;
use std::mem;

// Types

/// A state in a NFA (non-deterministic finite automaton).
pub struct State {
    state_type: StateType,

    // The states at the ends of the outgoing arrows of this state, if any
    // These states are referenced by their id, to find the actual states
    // one must go via the StateRegister.
    out: Vec<Option<u32>>
}

/// Types of states in the NFA's built from regular expressions.
pub enum StateType {
    // Connects to two states via empty/epsilon arrows
    Split,
    // The special state representing a match to the regex. No outgoing arrows.
    Match,
    // A literal character. Contains just a single arrow out with that character.
    Literal(char)
}

/// The state register contains and owns states. The states are accessed
/// through it, and it manages the lifetime of the states.
pub struct StateRegister {
    states: HashMap<u32, State>,
    current_id: u32
}

/// A non-deterministic finite automaton.
/// Defined by a state register which contains all the states, and the id of
/// the entry point (start state) of the NFA.
pub struct NFA {
    pub state_register: StateRegister,
    pub start_state: u32
}

/// A graph of states with a single input state. Represented
/// by the single input state and all end states. End states are
/// states with free (that is, `None`) out pointers.
/// The entire graph can be travesed knowing the start state by following
/// the out pointers until reaching all the end states.
/// States are represented by their unique id in reference to a
/// given StateRegister.
/// Represents a fragment of a finite automaton.
struct Fragment {
    start: u32,
    ends: Vec<u32>
}

// Main algorithms

/// Convert a regular expression to a NFA (non-deterministic finite
/// automaton).
/// Works by first converting the regular expression to postfix notation and
/// then applying Thompson's construction to that expression.
/// The NFA is represented by states in a state register, which is returned
/// together with the id of the start state of the NFA.
pub fn regex_to_nfa(regex: &str) -> NFA {
    let postfix_regex: String = regex_infix_to_postfix(regex);
    postfix_regex_to_nfa(&postfix_regex)
}

/// Use the Shunting-Yard algorithm to convert a regex written in
/// infix notation to a postifix regex.
/// Also the infix notation does not use a excplicit concatenation
/// operator, while the postfix notation does.
fn regex_infix_to_postfix(regex: &str) -> String {
    let mut output = String::new();
    let mut operator_stack: Vec<char> = Vec::new();
    let mut previous_char: Option<char> = None;
    let mut escaped: bool = false;

    // While iterating over the regex characters, handle a operator:
    // If not a opening paren, pop all operators from the operator stack to the
    // output that have higher precedence than the current operator.
    // Push the current operator to the operator stack.
    fn handle_current_char_is_operator(current_char: char,
                                       output: &mut String,
                                       operator_stack: &mut Vec<char>) {
        let opening_bracket_on_operator_stack_top: bool =
            operator_stack.last() == Some(&'(');

        if !opening_bracket_on_operator_stack_top {
            pop_into_while(
                operator_stack,
                output,
                &|c: char| {
                    precedence(c) > precedence(current_char)
                }
            );
        }

        operator_stack.push(current_char);
    };

    for character in regex.chars() {
        // For debugging
        // TODO: Remove
        println!("Current char: {}", character);
        println!("Current out: {}", output);
        println!("Current operator stack: {:?}", operator_stack);

        // If the current character is escaped then we push it directly to
        // the output without parsing it
        if escaped {
            output.push(character);
            continue;
        }

        // The escape character is pushed directly to the output, setting
        // escaped = true. This is because the escape character (\) is not
        // postfixed even in the postifix notation.
        if character == '\\' {
            output.push('\\');
            escaped = true;
            continue;
        }

        // The infix notation does not use a explicit concatenation character
        // (~), so if we see one here we should treat it as a literal character
        // but escape it in the output.
        if character == '~' {
            output.push('\\');
        }

        // Determine if the current character should be concatenated with the
        // previous. If so we temporarily act as if we were looking at a
        // concatenation character (~).
        let mut concatenate_previous: bool = false;

        if previous_char.is_some() {
            // Don't concatenate if current character is an operator or a
            // closing bracket.
            // Don't concatenate is the previous character was a alteration or
            // a opening bracket.
            concatenate_previous = !(
                ['*', '+', '?', '|', ')'].contains(&character) ||
                    ['|', '('].contains(&previous_char.unwrap())
            );
        }

        if concatenate_previous {
            // Pretend we are looking at a concatenation character (~)
            // instead of the currenet character.
            handle_current_char_is_operator(
                '~', &mut output, &mut operator_stack
            );
        }

        // Handle the current character
        match character {
            // Operators. Exluding concatenation which is not treated as
            // an operator in the infix notation so should match a literal
            // character here.
            '*' | '+' | '?' | '|' => {
                handle_current_char_is_operator(
                    character, &mut output, &mut operator_stack
                );
            },
            // Parentheses: grouping
            '(' => {
                operator_stack.push('(');
            },
            ')' => {
                pop_into_while(
                    &mut operator_stack,
                    &mut output,
                    &| c: char | c != '('
                );
                operator_stack.pop();  // Discard the opening bracket (
                // Discard the closing bracket )
            }
            // Default: Literal character
            _ => {
                output.push(character);
            }
        }

        // Set the previous character
        previous_char = Some(character);
    }

    // If there are any operators left on the operator stack, add them to
    // the output
    pop_into_while(
        &mut operator_stack,
        &mut output,
        &| _ | true
    );

    output
}

/// Convert a regular expression in postfix notation to a NFA.
pub fn postfix_regex_to_nfa(postfix_regex: &str) -> NFA {
    let mut register = StateRegister::new();
    let mut fragment_stack: Vec<Fragment> = Vec::new();

    for character in postfix_regex.chars() {
        match character {
            // Concatenation
            '~' => {
                // Connect the ends of fragment_1 to the start of fragment_2
                let fragment_2 = pop_or_panic(&mut fragment_stack, None);
                let fragment_1 = pop_or_panic(&mut fragment_stack, None);

                fragment_1.connect_ends(fragment_2.start, &mut register);

                // Fuse the two fragments together to a single fragment,
                // and push that to the stack
                let fused_fragment = Fragment {
                    start: fragment_1.start,
                    ends: fragment_2.ends
                };
                fragment_stack.push(fused_fragment);
            },
            // Or operation
            '|' => {
                let fragment_2 = pop_or_panic(&mut fragment_stack, None);
                let fragment_1 = pop_or_panic(&mut fragment_stack, None);

                // Create a new split state which has the start states of the
                // two fragments as the two choices.
                let split = register.new_split(
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
                fragment_stack.push(split_fragment);
            },
            // Zero or one
            '?' => {
                let fragment = pop_or_panic(&mut fragment_stack, None);
                let split_state = register.new_split(Some(fragment.start), None);

                let zero_or_one_fragment = Fragment {
                    start: split_state,
                    ends: [&fragment.ends[..], &[split_state]].concat()
                };

                fragment_stack.push(zero_or_one_fragment);
            },
            // Zero or more
            '*' => {
                let fragment = pop_or_panic(&mut fragment_stack, None);
                let split_state = register.new_split(Some(fragment.start), None);

                fragment.connect_ends(split_state, &mut register);

                let zero_or_more_fragment = Fragment {
                    start: split_state,
                    ends: vec![split_state]
                };

                fragment_stack.push(zero_or_more_fragment);
            },
            // One or more
            '+' => {
                let fragment = pop_or_panic(&mut fragment_stack, None);
                let split_state = register.new_split(Some(fragment.start), None);

                fragment.connect_ends(split_state, &mut register);

                let one_or_more_fragment = Fragment {
                    start: fragment.start,
                    ends: vec![split_state]
                };

                fragment_stack.push(one_or_more_fragment);
            },
            // Default: literal character
            _ => {
                // Push a fragment to the stack which contains just the
                // single literal state created by the literal character
                // with no out state
                let literal = register.new_literal(
                    character,
                    None
                );

                let single_literal_fragment = Fragment {
                    start: literal,
                    ends: vec![literal]
                };

                fragment_stack.push(single_literal_fragment);
            }
        }
    }

    // At the end of the loop there should be only one fragment left
    // in the stack for a valid postfix regex. Letting all the ends
    // of that fragment point to a matching state completes the
    // construction of the NFA. Returning the starting state the
    // NFA can be traversed following the out id's.
    let final_fragment_or_none = fragment_stack.pop();

    if fragment_stack.len() > 0 {
        // More than one fragment left means the passed postifx regex
        // was ill formed.
        panic!("Invalid postfix regex. More than one final fragment in construction.");
    }

    if final_fragment_or_none.is_some() {
        let final_fragment = final_fragment_or_none.unwrap();
        let match_state = register.match_state();
        final_fragment.connect_ends(match_state, &mut register);

        return NFA {
            state_register: register,
            start_state: final_fragment.start
        };
    }
    else {
        panic!("Unexpected empty stack after loop end!")
    }
}

// Implementations

impl NFA {
    /// Run the NFA with a given input string.
    /// The simulation can be in multiple NFA states at the same time.
    pub fn simulate(&self, input: &str) -> bool {
        let register: &StateRegister = &self.state_register;

        // Current states the NFA is in.
        // A hash set since it should not contain the same state twice.
        let mut current: HashSet<u32> = HashSet::new();
        insert_or_follow_split(
            &mut current,
            register.get_state(self.start_state),
            self.start_state,
            register
        );

        // States the NFA will be in after the current character
        let mut next: HashSet<u32> = HashSet::new();

        for character in input.chars() {
            // Use the current states to compute the next states given the
            // character in the input string
            for state_id in current.iter() {
                let state: &State = register.get_state(*state_id);

                match state.state_type {
                    StateType::Match => {
                        // Already in the match state. No need to iterate further
                        // as this means we have a match.
                        return true;
                    },
                    StateType::Literal(c) => {
                        if c == character {
                            let next_state_id = state.out[0].unwrap();
                            let next_state = register.get_state(next_state_id);
                            // The hash set guarantees this does not insert the
                            // state if it is already in `next`.
                            // Any split will be followed and the output
                            // states of that split will be added instead.
                            insert_or_follow_split(
                                &mut next, next_state, next_state_id, register
                            );
                        }
                    },
                    StateType::Split => {
                        // We should never reach a split here. That is the job of
                        // `insert_or_follow_split` to ensure.
                        panic!("Unexpected split found in `current`!");
                    }
                }
            }

            // Next becomes the new current, and the new next is initialized.
            mem::swap(&mut current, &mut next);
            next.clear();
        }

        // If the current states contain the match state after all characters
        // are iterated over then we have a match.
        for state in current {
            match register.get_state(state).state_type {
                StateType::Match => { return true; },
                _ => ()
            }
        }
        return false;
    }
}

impl State {
    fn remove_from_out(&mut self, id: u32) {
        self.out.retain(|val| *val != Some(id));
    }
}

impl StateRegister {
    fn new() -> StateRegister {
        StateRegister {
            states: HashMap::new(),
            current_id: 0
        }
    }

    /// Get a state by id, panic if it does not exist.
    fn get_state(&self, state_id: u32) -> &State {
        let ref_to_state_or_none = self.states.get(&state_id);

        if ref_to_state_or_none.is_some() {
            return ref_to_state_or_none.unwrap()
        }
        panic!("No state with id {}", state_id);
    }

    /// Get a mutable state by id, panic if it does not exist.
    fn get_mut_state(&mut self, state_id: u32) -> &mut State {
        let mut_ref_to_state_or_none = self.states.get_mut(&state_id);

        if mut_ref_to_state_or_none.is_some() {
            return mut_ref_to_state_or_none.unwrap()
        }
        panic!("No state with id {}", state_id);
    }

    /// Remove a state from the out states of a state
    fn remove_from_out(&mut self, state_id: u32, state_to_remove: u32) {
        let state = self.get_mut_state(state_id);
        state.remove_from_out(state_to_remove);
    }

    /// Connect all the unconnected (`None`) out
    /// connections of a state to some state.
    fn connect_dangling_outs_to_state(&mut self, state_id: u32, to_state: u32) {
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
    fn new_state(&mut self, state_type: StateType, out: Vec<Option<u32>>) -> u32 {
        self.states.insert(
            self.current_id,
            State { state_type, out }
        );

        // Increment the current id, this ensured the id of each state is
        // unique.
        self.current_id += 1;
        self.current_id - 1
    }

    fn new_literal(&mut self, contains: char, out_state: Option<u32>) -> u32 {
        self.new_state(
            StateType::Literal(contains),
            vec![out_state]
        )
    }

    fn new_split(&mut self, out_state_1: Option<u32>, out_state_2: Option<u32>) -> u32 {
        self.new_state(
            StateType::Split,
            vec![out_state_1, out_state_2]
        )
    }

    fn match_state(&mut self) -> u32 {
        self.new_state(
            StateType::Match,
            vec![]
        )
    }

    /// Get all states that point to a given state. Note that since
    /// the states only store the states they point to, not the states
    /// that point to it this requires iterating over all states.
    fn in_states(&self, state_id: u32) -> Vec<u32> {
        let mut result: Vec<u32> = Vec::new();

        for (id, state) in self.states.iter() {
            if state.out.iter().find(|&&x| x == Some(state_id)).is_some() {
                result.push(*id);
            }
        }

        result
    }

    /// Remove a state and all references to it from the register.
    /// This requires finding all states that point to the given state
    /// and removing the state from their out states list.
    fn remove_state(&mut self, state_to_remove: u32) {
        let all_ids: Vec<u32> = self.states.iter().map(|(id, _)| *id).collect();

        for id in all_ids {
            self.remove_from_out(id, state_to_remove);
        }

        self.states.remove(&state_to_remove);
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.state_type {
            StateType::Literal(c) => write!(f, "Literal ({}) ->", c),
            StateType::Split => write!(f, "Split <- () ->"),
            StateType::Match => write!(f, "MATCH ()")
        }
    }
}

impl Fragment {
    /// Attach all the unattached (`None`) outgoing lines of all the end states
    /// of the fragment to a given state.
    fn connect_ends(&self, to_state: u32, register: &mut StateRegister) {
        for end in &self.ends[..] {
            register.connect_dangling_outs_to_state(*end, to_state);
        }
    }
}

// Utility functions

/// Operator precedence for regex operators. Higher value
/// means higher precedence.
fn precedence(regex_operator: char) -> u32 {
    match regex_operator {
        '|' => 1,  // Alteration (or)
        '~' => 3,  // Concatenation (and)
        '*' => 4,  // Zero or more
        '+' => 4,  // One or more
        '?' => 4,  // Zero or one
        _ => { panic!("Invalid regex operator") }
    }
}

/// Pop into `to`. Returns `true` if there was anything to pop.
fn pop_into(from: &mut Vec<char>, to: &mut String) -> bool {
    let popped = from.pop();

    if popped.is_some() {
        to.push(popped.unwrap());
        return true;
    }
    return false;
}

/// Pop into `to` if predicate returns true on the last element
/// of `from`. Returns true if the move happened.
fn pop_into_if<F>(from: &mut Vec<char>, to: &mut String, predicate: &F) -> bool where
    F: Fn(char) -> bool {
    if from.len() == 0 {
        return false;
    }

    if predicate(from[from.len() - 1]) {
        return pop_into(from, to);
    }
    return false;
}

/// Pop into `to` while predicate returns true on the last element
/// of `from` or until the `from` vector is empty.
fn pop_into_while<F>(from: &mut Vec<char>, to: &mut String, predicate: &F) where
    F: Fn(char) -> bool {
    while pop_into_if(from, to, predicate) {}
}

/// Insert the state id of a state into `into`, unless the state is a split.
/// For a split follow the out arrows and call this function recursively on
/// the states they point to.
fn insert_or_follow_split(into: &mut HashSet<u32>, state: &State, state_id: u32, register: &StateRegister) {
    match state.state_type {
        StateType::Split => {
            let state_id_split_1 = state.out[0].unwrap();
            let state_split_1: &State = register.get_state(state_id_split_1);
            insert_or_follow_split(
                into,
                state_split_1,
                state_id_split_1,
                register
            );

            let state_id_split_2 = state.out[1].unwrap();
            let state_split_2: &State = register.get_state(state_id_split_2);
            insert_or_follow_split(
                into,
                state_split_2,
                state_id_split_2,
                register
            );
        },
        _ => {
            into.insert(state_id);
        }
    }
}

/// Traverse a NFA given by a start state and state register to which it
/// belongs, printing the nodes as we go along.
/// Only used for debugging the creation of the NFA.
pub fn print_nfa(start_id: u32, register: &StateRegister, visited: &mut HashSet<u32>) {
    let start = register.get_state(start_id);

    // To avoid infinte recursion we need to remember the states we
    // have already visited.
    if visited.contains(&start_id) {
        println!("{}", start);
        return;
    }

    visited.insert(start_id);

    match start.state_type {
        StateType::Match => {
            println!("{}", start);
        },
        _ => {
            println!("{}", start);
            for out_state_id in &start.out[..] {
                if out_state_id.is_some() {
                    print_nfa(out_state_id.unwrap(), register, visited);
                }
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
    match panic_message {
        Some(message) => panic!(message),
        _ => panic!("Attempted pop of empty vector.")
    }
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_nfa_matching_1() {
        let regex: &str = "(a|⻘)c";
        let nfa = regex_to_nfa(&regex);

        assert!(nfa.simulate("ac"));
        assert!(nfa.simulate("⻘c"));
        assert!(!nfa.simulate("a"));  // Missing c
        assert!(!nfa.simulate("c"));  // Missing first char
        assert!(!nfa.simulate("xc"));  // Wrong first char
    }

    #[test]
    fn test_regex_nfa_matching_2() {
        let regex: &str = "(a|b)*c+";
        let nfa = regex_to_nfa(&regex);

        assert!(nfa.simulate("ac"));
        assert!(nfa.simulate("c"));
        assert!(nfa.simulate("aaaac"));
        assert!(nfa.simulate("accccc"));
        assert!(nfa.simulate("bc"));
        assert!(nfa.simulate("abc"));  // Both characters allowed in zero or more
        assert!(!nfa.simulate("b"));  // Too few c
        assert!(!nfa.simulate(""));  // Too few c
    }

    #[test]
    fn test_postfix_regex_nfa_matching_1() {
        // Test or and concatenation
        let postfix_regex: &str = "abc|~";
        let nfa = postfix_regex_to_nfa(&postfix_regex);

        assert!(nfa.simulate("ab"));
        assert!(nfa.simulate("ac"));
        assert!(nfa.simulate("abx"));
        assert!(!nfa.simulate("a"));
        assert!(!nfa.simulate("aa"));
        assert!(!nfa.simulate(""));
    }

    #[test]
    fn test_postfix_regex_nfa_matching_2() {
        // Test zero or more, one or more, zero or one
        // Test unicode support
        let postfix_regex: &str = "a*ø?~⻘+~";
        let nfa = postfix_regex_to_nfa(&postfix_regex);

        assert!(nfa.simulate("a⻘"));
        assert!(nfa.simulate("aø⻘"));
        assert!(nfa.simulate("⻘"));
        assert!(nfa.simulate("ø⻘"));
        assert!(nfa.simulate("aaaaaaaaaaaa⻘⻘⻘⻘⻘⻘"));
        assert!(!nfa.simulate("aøø⻘"));  // Too many ø
        assert!(!nfa.simulate("aø"));  // Too few ⻘
    }

    #[test]
    fn test_regex_infix_to_postfix() {
        assert_eq!(
            &regex_infix_to_postfix("a(b|c)"),
            "abc|~"
        );
        assert_eq!(
            &regex_infix_to_postfix("(a|⻘)c"),
            "a⻘|c~"
        );
        assert_eq!(
            &regex_infix_to_postfix("(a|b)*c+"),
            "ab|*c+~"
        );
    }
}

