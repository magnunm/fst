use std::io::{self, BufRead};
use std::collections::HashSet;

mod regex;


fn main() -> io::Result<()> {
    let regex: &str = "(a|⻘)c+";
    let nfa = regex::regex_to_nfa(regex);

    regex::print_nfa(nfa.start_state, &nfa.state_register, &mut HashSet::new());

    let stdin = io::stdin();
    let mut input = stdin.lock().lines().next().unwrap().unwrap();

    while input != "q" {
        let match_greedy = nfa.simulate(&input, true);

        println!("Got index: {}", match_greedy);

        input = stdin.lock().lines().next().unwrap().unwrap();
    }
    Ok(())
}
