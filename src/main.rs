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
        .get_matches();

    let pattern = matches.value_of("PATTERN").unwrap();
    let regex = match regex::Regex::new(&pattern, true) {
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidInput, e)),
        Ok(r) => r
    };

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
            if matches.is_present("color") {
                if match_end == bytes_read {
                    println!("{}{}",
                             &line[..match_start],
                             Red.paint(&line[match_start..match_end]))
                } else {
                    println!("{}{}{}",
                             &line[..match_start],
                             Red.paint(&line[match_start..match_end]),
                             &line[match_end..(bytes_read - 1)]) // Strip newline
                }
            } else {
                println!("{}", &line[..(bytes_read - 1)])
            }
        }

        line.clear();
    }

    Ok(())
}
