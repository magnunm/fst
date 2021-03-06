use std::io::{self, BufRead};

mod regex;


fn main() -> io::Result<()> {
    let regex = regex::Regex::new("(a|⻘)c+", true);

    let stdin = io::stdin();
    let mut input = stdin.lock().lines().next().unwrap().unwrap();

    while input != "q" {
        let (match_start, match_end) = regex.match_substring(&input);

        println!("Match from {} to {}", match_start, match_end);

        input = stdin.lock().lines().next().unwrap().unwrap();
    }
    Ok(())
}
