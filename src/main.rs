use std::io::{self, BufRead};
use std::string::String;

mod regex;


fn main() -> io::Result<()> {
    let postfix_regex: &str = "abc|.";
    let (register, start_state_for_nfa) = regex::postfix_regex_to_nfa(&postfix_regex);

    regex::print_nfa(start_state_for_nfa, &register);

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
