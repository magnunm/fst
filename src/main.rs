use std::io::{self, BufRead};
use std::collections::HashSet;

mod regex;


fn main() -> io::Result<()> {
    let regex: &str = "(a|⻘)*c+";
    let nfa = regex::regex_to_nfa(&regex);

    regex::print_nfa(nfa.start_state, &nfa.state_register, &mut HashSet::new());

    let stdin = io::stdin();
    let input = stdin.lock().lines().next().unwrap().unwrap();

    let is_match = nfa.simulate(&input);

    if is_match {
        println!("Match!");
    }
    else {
        println!("No match!");
    }
    Ok(())
}
