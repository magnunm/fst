use std::str;
use std::vec::Vec;
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;
use std::mem;

pub enum StateType {
    Split,
    Match,
    Literal(char)
}

pub struct State {
    state_type: StateType,

    // The states at the ends of the outgoing arrows of this state, if any
    // These states are referenced by their id, to find the actual states
    // one must go via the StateRegister.
    out: Vec<Option<u32>>,

    // lastlist: u32
}

impl State {
    fn remove_from_out(&mut self, id: u32) {
        self.out.retain(|val| *val != Some(id));
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

/// The state register contains and owns states. The states are accessed
/// through it, and it manages the lifetime of the states.
pub struct StateRegister {
    states: HashMap<u32, State>,
    current_id: u32
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


impl Fragment {
    /// Attach all the unattached (`None`) outgoing lines of all the end states
    /// of the fragment to a given state.
    fn connect_ends(&self, to_state: u32, register: &mut StateRegister) {
        for end in &self.ends[..] {
            register.connect_dangling_outs_to_state(*end, to_state);
        }
    }
}

/// Convert a regular expression in postfix notation to a NFA.
/// The NFA is represented by states in a state register, which is returned
/// together with the id of the start state of the NFA.
pub fn postfix_regex_to_nfa(postfix_regex: &str) -> (StateRegister, u32) {
    let mut register = StateRegister::new();
    let mut fragment_stack: Vec<Fragment> = Vec::new();

    for character in postfix_regex.chars() {
        match character {
            // Concatenation
            '.' => {
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

    if final_fragment_or_none.is_some() {
        let final_fragment = final_fragment_or_none.unwrap();
        let match_state = register.match_state();
        final_fragment.connect_ends(match_state, &mut register);

        return (register, final_fragment.start);
    }
    else {
        panic!("Unexpected empty stack after loop end!")
    }
}

/// Traverse a NFA given by a start state and state register to which it
/// belongs, printing the nodes as we go along.
/// For debugging the creation of the NFA.
pub fn print_nfa(start_id: u32, register: &StateRegister) {
    let start = register.get_state(start_id);

    match start.state_type {
        StateType::Match => {
            println!("{}", start);
        },
        _ => {
            println!("{}", start);
            for out_state_id in &start.out[..] {
                if out_state_id.is_some() {
                    print_nfa(out_state_id.unwrap(), register);
                }
            }
        }
    }
}

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

pub fn simulate_nfa(input: &str, start: u32, register: &StateRegister) -> bool {
    // Current states the NFA is in.
    // A hash set since it should not contain the same state twice.
    let mut current: HashSet<u32> = HashSet::new();
    insert_or_follow_split(
        &mut current, register.get_state(start), start, register
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

/// pop the last element from a vector, panic if empty
fn pop_or_panic<T>(vector: &mut Vec<T>, panic_message: Option<&'static str>) -> T {
    let result: Option<T> = vector.pop();
    if result.is_some() {
        return result.unwrap();
    }
    match panic_message {
        Some(message) => panic!(panic_message),
        _ => panic!("Attempted pop of empty vector.")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_postfix_regex_nfa_matching_1() {
        // Test or and concatenation
        let postfix_regex: &str = "abc|.";
        let (register, start_state_for_nfa) = postfix_regex_to_nfa(&postfix_regex);
        let match_postfix_regex = | input: &str | -> bool {
            simulate_nfa(&input, start_state_for_nfa, &register)
        };

        assert!(match_postfix_regex("ab"));
        assert!(match_postfix_regex("ac"));
        assert!(match_postfix_regex("abx"));
        assert!(!match_postfix_regex("a"));
        assert!(!match_postfix_regex("aa"));
        assert!(!match_postfix_regex(""));
    }

    #[test]
    fn test_postfix_regex_nfa_matching_2() {
        // Test zero or more, one or more, zero or one
        // Test unicode support
        let postfix_regex: &str = "a*ø?.⻘+.";
        let (register, start_state_for_nfa) = postfix_regex_to_nfa(&postfix_regex);
        let match_postfix_regex = | input: &str | -> bool {
            simulate_nfa(&input, start_state_for_nfa, &register)
        };

        assert!(match_postfix_regex("a⻘"));
        assert!(match_postfix_regex("aø⻘"));
        assert!(match_postfix_regex("⻘"));
        assert!(match_postfix_regex("ø⻘"));
        assert!(match_postfix_regex("aaaaaaaaaaaa⻘⻘⻘⻘⻘⻘"));
        assert!(!match_postfix_regex("aøø⻘"));  // Too many ø
        assert!(!match_postfix_regex("aø"));  // Too few ⻘
    }
}

