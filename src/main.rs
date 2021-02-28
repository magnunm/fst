use std::io::{self, BufRead};
use std::collections::HashSet;

mod regex;


fn main() -> io::Result<()> {
    let regex: &str = "(a|⻘)*c+";
    let (register, start_state_for_nfa) = regex::regex_to_nfa(&regex);

    regex::print_nfa(start_state_for_nfa, &register, &mut HashSet::new());

    let stdin = io::stdin();
    let input = stdin.lock().lines().next().unwrap().unwrap();

    let is_match = regex::simulate_nfa(&input, start_state_for_nfa, &register);

    if is_match {
        println!("Match!");
    }
    else {
        println!("No match!");
    }
    Ok(())
}
