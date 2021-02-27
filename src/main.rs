use std::io;
use std::string::String;

mod regex;


fn main() -> io::Result<()> {
    let postfix_regex: &str = "abc|.";
    let (register, start_state_for_nfa) = regex::postfix_regex_to_nfa(&postfix_regex);

    regex::print_nfa(start_state_for_nfa, &register);
    Ok(())
}
