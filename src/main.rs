use std::io::{self, BufReader, BufRead};
use std::fs::File;
use walkdir::{DirEntry, WalkDir};

use clap::{Arg, App};
use ansi_term::Colour::{Red, Blue};

mod regex;


fn main() -> io::Result<()> {
    let matches = App::new("fst")
        .about("A stream filter. Supported OPERATION's are: p (print matching lines), ip (inverse print, print non-matching lines), m (print only matching substrings), im (print the matching lines with the matching substrings removed) and c (count matching lines). Default is p.")
        .arg(Arg::with_name("PATTERN")
             .help("The pattern to match")
             .required(true)
             .index(1))
        .arg(Arg::with_name("FILE")
             .help("The file to search. If none read stdin. If recursive search then this is the directory, or current directory if none.")
             .index(2))
        .arg(Arg::with_name("black-and-white")
             .short("b")
             .long("black-and-white")
             .help("Disable colors"))
        .arg(Arg::with_name("recursive")
             .short("r")
             .long("recursive")
             .help("Search all files in the directory FILE recursively."))
        .arg(Arg::with_name("operation")
             .short("o")
             .long("operation")
             .value_name("OPERATION")
             .takes_value(true)
             .help("Operation to preform on the lines of the FILE."))
        .get_matches();

    let pattern = matches.value_of("PATTERN").unwrap();
    let regex = match regex::Regex::new(&pattern) {
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidInput, e)),
        Ok(r) => r
    };

    let color = !matches.is_present("black-and-white");
    let operation = matches.value_of("operation").unwrap_or("p");
    let operation_func = get_operation_func(operation)?;

    let recursive = matches.is_present("recursive");
    if recursive {
        let directory_name = matches.value_of("FILE").unwrap_or(".");
        for entry in WalkDir::new(directory_name)
            .follow_links(true)
            .into_iter()
            .filter_entry(|e| !is_hidden(e))
            .filter_map(Result::ok)
            .filter(|e| !e.file_type().is_dir()) {
                let path = entry.path().to_str().unwrap();
                let file = File::open(&path)?;
                let mut reader: Box<dyn BufRead> = Box::new(BufReader::new(file));
                let prefix = &format!("{}:", path);
                let count = apply_operation_to_reader(
                    &mut reader,
                    &regex,
                    operation_func,
                    color,
                    prefix,
                )?;
                if operation == "c" && count > 0 { println!("{} {}", prefix, count);}
            }
        return Ok(());
    }

    let mut reader: Box<dyn BufRead>;

    if let Some(file_name) = matches.value_of("FILE") {
        let file = File::open(&file_name)?;
        reader = Box::new(BufReader::new(file));
    } else {
        let stdin = io::stdin();
        reader = Box::new(BufReader::new(stdin));
    }

    let count = apply_operation_to_reader(&mut reader, &regex, operation_func, color, "")?;
    if operation == "c" { println!("{}", count);}

    Ok(())
}

fn apply_operation_to_reader(
    reader: &mut Box<dyn BufRead>,
    regex: &regex::Regex,
    operation_func: fn(&str, usize, usize, usize, bool, &str) -> (),
    color: bool,
    prepend: &str,
) -> io::Result<i32> {
    let mut line = String::new();
    let mut line_no_newline: &str;
    let mut num_matching_lines: i32 = 0;

    loop {
        let bytes_read = reader.read_line(&mut line)?;
        if bytes_read == 0 { break; }

        // Strip newline from string used to match against
        line_no_newline = &line[..(bytes_read - 1)];
        let (match_start, match_end) = regex.match_substring(&line_no_newline);

        if match_start != match_end {
            num_matching_lines += 1;
        }

        operation_func(&line_no_newline, match_start, match_end, bytes_read - 1, color, prepend);
        line.clear();
    }
    Ok(num_matching_lines)
}

/// Print the given line if it contains a nonempty matching substring.
fn print_line_with_match(
    line: &str,
    match_start: usize,
    match_end: usize,
    line_length: usize,
    color: bool,
    prepend: &str,
) {
    if match_start == match_end { return; }

    if color {
        print!("{}", Blue.paint(prepend));
    } else {
        print!("{}", prepend);
    }

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
    _color: bool,
    prepend: &str,
) {
    if match_start == match_end {
        println!("{}", &line[..line_length])
    }
}

/// Print the mathcing substring of a matching line.
fn print_matching_substring(
    line: &str,
    match_start: usize,
    match_end: usize,
    _line_length: usize,
    color: bool,
    prepend: &str,
) {
    if match_start == match_end { return; }
    if color {
        print!("{}", Blue.paint(prepend));
    } else {
        print!("{}", prepend);
    }
    println!("{}", &line[match_start..match_end])
}

/// Print all but the mathcing substring of a matching line.
fn print_all_but_matching_substring(
    line: &str,
    match_start: usize,
    match_end: usize,
    line_length: usize,
    color: bool,
    prepend: &str,
) {
    if match_start == match_end { return; }

    if color {
        print!("{}", Blue.paint(prepend));
    } else {
        print!("{}", prepend);
    }

    println!("{}{}",
             &line[..match_start],
             &line[match_end..line_length]);
}

fn get_operation_func(operation: &str) -> io::Result<fn(&str, usize, usize, usize, bool, &str) -> ()> {
    return match operation {
        "p" => Ok(print_line_with_match),
        "ip" => Ok(print_line_without_match),
        "m" => Ok(print_matching_substring),
        "im" => Ok(print_all_but_matching_substring),
        "c" => Ok(|_, _, _, _, _, _| {}),
        _ => return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("Unsupported operation '{}'", operation)))
    }
}

fn is_hidden(entry: &DirEntry) -> bool {
    entry.file_name()
         .to_str()
         .map(|s| s.starts_with(".") && s != ".")
         .unwrap_or(false)
}
