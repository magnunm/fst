use std::fmt;
use std::vec::Vec;

/// A non-deterministic finite automaton.
/// Defined by a state register which contains all the states, and the id of
/// the entry point (start state) of the NFA.
pub struct NFA<'a> {
    pub state_register: StateRegister<'a>,
    pub start_state: usize,
}

impl<'a> NFA<'a> {
    /// Simulate the NFA with a given input string.
    ///
    /// The simulation can be in multiple NFA states at the same time.
    /// `greedy` controls wether or not we will match the tail of the
    /// regex greedily or not. If true the returned byte index will be
    /// the first character after the longest matching substring in
    /// `input`, if `false` it will be the index after the shortest
    /// matching substring in `input`.
    /// If no match returns `None`.
    pub fn simulate(&self, input: &str, greedy: bool) -> Option<usize> {
        let mut simulation = NFASimulation::new(&self);

        // Char byte index of the character after the longest matching substring found this far, or
        // `None` if no such substring has been found. The empty string is also considered a valid
        // substring, so a result of `Some(0)` is a match on the empty string.
        let mut first_non_matching_char_index: Option<usize> = None;

        for (byte_index, character) in input.char_indices() {
            if simulation.in_match_state {
                first_non_matching_char_index = Some(byte_index);

                if !greedy {
                    return first_non_matching_char_index;
                }
            }

            // If there are no surviving states there is no need to continue iterating over the
            // characters.
            if simulation.current_states.is_empty() {
                return first_non_matching_char_index;
            }

            simulation.update_states(character);
        }

        if simulation.in_match_state {
            first_non_matching_char_index = Some(input.len());
        }

        first_non_matching_char_index
    }
}

struct NFASimulation<'a> {
    nfa: &'a NFA<'a>,
    // Current states the NFA simulation is in.
    // Even though it should not contain the same state twice a
    // vec is more preformant than a hash-set.
    current_states: Vec<usize>,
    // Has the match state been reached?
    in_match_state: bool,
}

impl<'a> NFASimulation<'a> {
    fn new(nfa: &'a NFA) -> NFASimulation<'a> {
        let mut new_simulation = NFASimulation {
            nfa,
            current_states: Vec::new(),
            in_match_state: false,
        };

        // Initialize the current states of the simulation with the
        // starting states
        let mut inital_states = NFASimulationNextStates::new(nfa, 10);
        inital_states.insert_or_follow_split(
            nfa.state_register.get_state(nfa.start_state),
            nfa.start_state,
        );
        new_simulation.current_states = inital_states.states;
        new_simulation.in_match_state = inital_states.contains_match_state;

        new_simulation
    }

    /// Use the current states to compute the next states given the
    /// character.
    fn update_states(&mut self, character: char) {
        let mut next_states = NFASimulationNextStates::new(self.nfa, 2 * self.current_states.len());

        for state_id in &self.current_states {
            let state: &State = self.nfa.state_register.get_state(*state_id);
            next_states.update(state, character);
        }

        self.current_states = next_states.states;
        self.in_match_state = next_states.contains_match_state;
    }
}

struct NFASimulationNextStates<'a> {
    nfa: &'a NFA<'a>,
    states: Vec<usize>,
    contains_match_state: bool,
}

impl<'a> NFASimulationNextStates<'a> {
    fn new(nfa: &'a NFA, state_capacity: usize) -> NFASimulationNextStates<'a> {
        NFASimulationNextStates {
            nfa,
            states: Vec::with_capacity(state_capacity),
            contains_match_state: false,
        }
    }

    /// Update the list of next states with the states reached by
    /// reading `character` in the given `state`.
    fn update(&mut self, state: &State, character: char) {
        match &state.state_type {
            // Literal: follow out if current matches literal's char
            StateType::Literal(c) => {
                if *c == character {
                    self.follow_first_out_arrow(state);
                }
            }
            // With a dot state we follow the output arrow
            // regardless of what the current character is
            StateType::Dot => {
                self.follow_first_out_arrow(state);
            }
            // With a bracket state we follow the output arrow if
            // the current character matches the bracket.
            StateType::Bracket(bracketed) => {
                if matches_bracket(character, bracketed) {
                    self.follow_first_out_arrow(state);
                }
            }
            StateType::Split => {
                // We should never reach a split here. The job of
                // `insert_or_follow_split` is to handle those.
                panic!("Unexpected `Split` passed to `update`!");
            }
            StateType::Match => {
                // `insert_of_follow_split` also handles match states.
                panic!("Unexpected `Match` passed to `update`!");
            }
        }
    }

    /// Follow the first out arrow of a state and insert the state at
    /// the end of it into the next states. Any split will be followed
    /// and the output states of that split will be added instead.
    /// TODO: Is it worth it to check if the state is already in next?
    fn follow_first_out_arrow(&mut self, state: &State) {
        let next_state_id = state.out[0].unwrap();
        let next_state = self.nfa.state_register.get_state(next_state_id);
        self.insert_or_follow_split(next_state, next_state_id);
    }

    /// Insert the state id into the list of next states, unless:
    /// - A split state: follow the out arrows and recurse
    /// - A match state
    fn insert_or_follow_split(&mut self, state: &State, state_id: usize) {
        match state.state_type {
            StateType::Split => {
                // Filter out `None` arrows
                for state_out_id in state.out.iter().flat_map(|id| *id) {
                    let state_out: &State = self.nfa.state_register.get_state(state_out_id);

                    self.insert_or_follow_split(state_out, state_out_id);
                }
            }
            StateType::Match => {
                self.contains_match_state = true;
            }
            _ => {
                self.states.push(state_id);
            }
        }
    }
}

/// The state register contains and owns states. The states are accessed
/// through it, and it manages the lifetime of the states.
pub struct StateRegister<'a> {
    states: Vec<State<'a>>,
    current_id: usize,
}

impl<'a> StateRegister<'a> {
    pub fn new() -> StateRegister<'a> {
        StateRegister {
            states: Vec::new(),
            current_id: 0,
        }
    }

    /// Get a state by id, panic if it does not exist.
    fn get_state(&self, state_id: usize) -> &State<'a> {
        let ref_to_state_or_none = self.states.get(state_id);

        if ref_to_state_or_none.is_some() {
            return ref_to_state_or_none.unwrap();
        }
        panic!("No state with id {}", state_id);
    }

    /// Get a mutable state by id, panic if it does not exist.
    fn get_mut_state(&mut self, state_id: usize) -> &mut State<'a> {
        let mut_ref_to_state_or_none = self.states.get_mut(state_id);

        if mut_ref_to_state_or_none.is_some() {
            return mut_ref_to_state_or_none.unwrap();
        }
        panic!("No state with id {}", state_id);
    }

    /// Connect all the unconnected (`None`) out
    /// connections of a state to some state.
    pub fn connect_dangling_outs_to_state(&mut self, state_id: usize, to_state: usize) {
        let mut state = self.get_mut_state(state_id);

        state.out = state
            .out
            .iter()
            .map(|val| {
                if val.is_some() {
                    return *val;
                }
                return Some(to_state);
            })
            .collect();
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

    pub fn new_literal(&mut self, contains: char, out_state: Option<usize>) -> usize {
        self.new_state(StateType::Literal(contains), vec![out_state])
    }

    pub fn new_dot(&mut self, out_state: Option<usize>) -> usize {
        self.new_state(StateType::Dot, vec![out_state])
    }

    pub fn new_bracket(&mut self, contains: &'a str, out_state: Option<usize>) -> usize {
        self.new_state(StateType::Bracket(contains), vec![out_state])
    }

    pub fn new_split(&mut self, out_state_1: Option<usize>, out_state_2: Option<usize>) -> usize {
        self.new_state(StateType::Split, vec![out_state_1, out_state_2])
    }

    pub fn match_state(&mut self) -> usize {
        self.new_state(StateType::Match, vec![])
    }
}

/// A state in a NFA (non-deterministic finite automaton).
struct State<'a> {
    state_type: StateType<'a>,

    // The states at the ends of the outgoing arrows of this state, if any
    // These states are referenced by their id, to find the actual states
    // one must go via the StateRegister.
    out: Vec<Option<usize>>,
}

impl<'a> fmt::Display for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.state_type {
            StateType::Literal(c) => write!(f, "Literal ({}) ->", c),
            StateType::Dot => write!(f, "Dot (.) ->"),
            StateType::Bracket(bracketed) => write!(f, "Bracket ([{}]) ->", &bracketed),
            StateType::Split => write!(f, "Split <- () ->"),
            StateType::Match => write!(f, "MATCH ()"),
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
        } else {
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
        return range_start <= character && character <= range_end;
    }
    panic!("Invalid range: {}-{}", range_start, range_end)
}
