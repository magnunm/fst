use std::io::{self, BufRead};
use std::collections::HashSet;

mod regex;


fn main() -> io::Result<()> {
    let regex: &str = "(a|⻘)c";
    let postfix_regex: String = regex::regex_infix_to_postfix(regex);
    let nfa = regex::postfix_regex_to_nfa(&postfix_regex);

    regex::print_nfa(nfa.start_state, &nfa.state_register, &mut HashSet::new());

    let stdin = io::stdin();
    let mut input = stdin.lock().lines().next().unwrap().unwrap();

    while input != "q" {
        let match_greedy = nfa.simulate(&input, false);

        println!("Got index: {}", match_greedy);

        input = stdin.lock().lines().next().unwrap().unwrap();
    }
    Ok(())
}
