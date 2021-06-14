use std::io::{self, BufReader, BufRead};
use std::fs::File;

use clap::{Arg, App};
use ansi_term::Colour::Red;

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
        .arg(Arg::with_name("color")
             .short("c")
             .long("color")
             .help("Enable colors"))
        .arg(Arg::with_name("greedy")
             .short("g")
             .long("greedy")
             .help("Match greedily"))
        .get_matches();

    let color = matches.is_present("color");

    let pattern = matches.value_of("PATTERN").unwrap();
    let greedy = matches.is_present("greedy");
    let regex = match regex::Regex::new(&pattern, greedy) {
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidInput, e)),
        Ok(r) => r
    };

    let mut reader: Box<dyn BufRead>;

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

        print_line_with_match(&line, match_start, match_end, bytes_read, color);

        line.clear();
    }

    Ok(())
}


fn print_line_with_match(
    line: &str,
    match_start: usize,
    match_end: usize,
    line_length: usize,
    color: bool
) {
    if match_start == match_end { return; }

    if color {
        if match_end == line_length {
            println!("{}{}",
                     &line[..match_start],
                     Red.paint(&line[match_start..match_end]));
            return;
        }
        println!("{}{}{}",
                 &line[..match_start],
                 Red.paint(&line[match_start..match_end]),
                 &line[match_end..(line_length - 1)]); // Strip newline
        return;

    }
    println!("{}", &line[..(line_length - 1)])
}
