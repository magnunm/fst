use std::io::{self, BufRead};
use std::collections::HashSet;

mod regex;


fn main() -> io::Result<()> {
    // A possible email regex
    let regex: &str = "[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+";
    let postfix_regex: String = regex::regex_infix_to_postfix(regex);
    let nfa = regex::postfix_regex_to_nfa(&postfix_regex);

    regex::print_nfa(nfa.start_state, &nfa.state_register, &mut HashSet::new());

    let stdin = io::stdin();
    let mut input = stdin.lock().lines().next().unwrap().unwrap();

    while input != "q" {
        let is_match = nfa.simulate(&input);

        if is_match {
            println!("Match!");
        }
        else {
            println!("No match!");
        }

        input = stdin.lock().lines().next().unwrap().unwrap();
    }
    Ok(())
}
