use std::io::{self, BufReader, BufRead};
use std::fs::File;

use clap::{Arg, App};
mod regex;


fn main() -> io::Result<()> {
    let matches = App::new("fst")
        .about("A stream filter")
        .arg(Arg::with_name("PATTERN")
             .help("The pattern to match")
             .required(true)
             .index(1))
        .arg(Arg::with_name("FILE")
             .help("The file to search. If none read stdin.")
             .index(2))
        .get_matches();

    let pattern = matches.value_of("PATTERN").unwrap();
    let regex = regex::Regex::new(&pattern, true).unwrap();

    let mut reader: Box<BufRead>;

    if let Some(file_name) = matches.value_of("FILE") {
        let file = File::open(&file_name)?;
        reader = Box::new(BufReader::new(file));
    } else {
        let stdin = io::stdin();
        reader = Box::new(BufReader::new(stdin));
    }

    let mut line = String::new();

    loop {
        let bytes_read = reader.read_line(&mut line)?;
        if bytes_read == 0 { break; }

        let (match_start, match_end) = regex.match_substring(&line);

        if match_start != match_end {
            // Strip final newline character
            println!("{}", &line[..(bytes_read - 1)])
        }

        line.clear();
    }

    Ok(())
}
