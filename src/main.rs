use std::io::{self, BufReader, BufRead};
use std::fs::File;

use clap::{Arg, App};
use ansi_term::Colour::Red;

mod regex;


fn main() -> io::Result<()> {
    let matches = App::new("fst")
        .about("A stream filter. Supported OPERATION's that can be applied are: p (print matching lines), ip (inverse print, print non-matching lines), m (print only matching substrings), im (print the matching lines with the matching substrings removed) and c (count matching lines). Default is p.")
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
        .arg(Arg::with_name("lazy")
             .short("l")
             .long("lazy")
             .help("Match the tail of the regex lazily. Somewhat faster."))
        .arg(Arg::with_name("operation")
             .short("o")
             .long("operation")
             .value_name("OPERATION")
             .takes_value(true)
             .help("Operation to preform on the lines of the FILE."))
        .get_matches();

    let pattern = matches.value_of("PATTERN").unwrap();
    let greedy = !matches.is_present("lazy");
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
    let mut line_no_newline: &str;

    let color = matches.is_present("color");
    let operation = matches.value_of("operation").unwrap_or("p");

    let mut num_matching_lines = 0;

    loop {
        let bytes_read = reader.read_line(&mut line)?;
        if bytes_read == 0 { break; }

        // Strip newline from string used to match against
        line_no_newline = &line[..(bytes_read - 1)];

        let (match_start, match_end) = regex.match_substring(&line_no_newline);

        match operation {
            "p" => print_line_with_match(
                &line_no_newline, match_start, match_end, bytes_read - 1, color
            ),
            "ip" => print_line_without_match(
                &line_no_newline, match_start, match_end, bytes_read - 1
            ),
            "m" => print_matching_substring(
                &line_no_newline, match_start, match_end
            ),
            "im" => print_all_but_matching_substring(
                &line_no_newline, match_start, match_end, bytes_read - 1
            ),
            "c" => {
                if match_start != match_end {
                    num_matching_lines += 1;
                }
            },
            _ => return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Unsupported operation '{}'", operation)))
        }

        line.clear();
    }

    if operation == "c" {
        println!("{}", num_matching_lines);
    }

    Ok(())
}

/// Print the given line if it contains a nonempty matching substring.
fn print_line_with_match(
    line: &str,
    match_start: usize,
    match_end: usize,
    line_length: usize,
    color: bool
) {
    if match_start == match_end { return; }

    if color {
        println!("{}{}{}",
                 &line[..match_start],
                 Red.paint(&line[match_start..match_end]),
                 &line[match_end..line_length]);
        return;

    }
    println!("{}", &line[..line_length])
}

/// Print the given line if it does not contain a nonempty matching substring.
fn print_line_without_match(
    line: &str,
    match_start: usize,
    match_end: usize,
    line_length: usize,
) {
    if match_start == match_end {
        println!("{}", &line[..line_length])
    }
}

/// Print the mathcing substring of a matching line.
fn print_matching_substring(
    line: &str,
    match_start: usize,
    match_end: usize
) {
    if match_start == match_end { return; }
    println!("{}", &line[match_start..match_end])
}

/// Print all but the mathcing substring of a matching line.
fn print_all_but_matching_substring(
    line: &str,
    match_start: usize,
    match_end: usize,
    line_length: usize
) {
    if match_start == match_end { return; }

    println!("{}{}",
             &line[..match_start],
             &line[match_end..line_length]);
}
