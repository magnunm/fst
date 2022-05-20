use std::boxed::Box;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use walkdir::{DirEntry, WalkDir};

use clap::{App, Arg};

mod operations;
mod regex;

fn main() -> io::Result<()> {
    let matches = App::new("fst")
        .about(concat!(
            "A stream filter. Supported OPERATION's are: \n",
            "p  (print matching lines) \n",
            "ip (inverse print, print non-matching lines) \n",
            "m  (print only matching substrings) \n",
            "im (print the matching lines with the matching substrings removed) \n",
            "c  (count matching lines) \n",
            "Default is p.",
        ))
        .arg(
            Arg::with_name("PATTERN")
                .help("The pattern to match")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("FILE")
                .help(concat!(
                    "The file to search. If none read stdin. If recursive search then this is ",
                    "the directory, or current directory if none.",
                ))
                .index(2),
        )
        .arg(
            Arg::with_name("black-and-white")
                .short("b")
                .long("black-and-white")
                .help("Disable colors"),
        )
        .arg(
            Arg::with_name("recursive")
                .short("r")
                .long("recursive")
                .help("Search all files in the directory FILE recursively."),
        )
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Verbose error messaging."),
        )
        .arg(
            Arg::with_name("operation")
                .short("o")
                .long("operation")
                .value_name("OPERATION")
                .takes_value(true)
                .help("Operation to preform on the lines of the FILE."),
        )
        .get_matches();

    let pattern = matches.value_of("PATTERN").unwrap();
    let regex = match regex::Regex::new(&pattern) {
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidInput, e)),
        Ok(r) => r,
    };

    let mut operation = get_operation(
        matches.value_of("operation").unwrap_or("p"),
        !matches.is_present("black-and-white"),
    )?;

    if matches.is_present("recursive") {
        let directory_name = matches.value_of("FILE").unwrap_or(".");
        recursive_search(
            directory_name,
            &regex,
            &mut operation,
            matches.is_present("verbose"),
        )?;
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

    apply_operation_to_reader(&mut reader, &regex, &mut operation, "")?;

    let final_report = operation.final_report();
    if final_report.len() > 0 {
        println!("{}", operation.final_report());
    }

    Ok(())
}

fn recursive_search(
    directory_name: &str,
    regex: &regex::Regex,
    operation: &mut Box<dyn operations::Operation>,
    verbose: bool,
) -> io::Result<()> {
    for entry in WalkDir::new(directory_name)
        .follow_links(true)
        .into_iter()
        .filter_entry(|e| !is_hidden(e))
        .filter_map(Result::ok)
        .filter(|e| !e.file_type().is_dir())
    {
        let path = entry.path().to_str().unwrap();
        let file = File::open(&path)?;
        let mut reader: Box<dyn BufRead> = Box::new(BufReader::new(file));
        let prefix = &format!("{}:", path);

        match apply_operation_to_reader(&mut reader, regex, operation, prefix) {
            Ok(()) => {
                let report = operation.final_report();
                if report.len() > 0 {
                    println!("{} {}", prefix, report);
                }
            }
            Err(message) => {
                if verbose {
                    println!("Error in {} {}", prefix, message);
                }
            }
        }
    }
    Ok(())
}

fn apply_operation_to_reader(
    reader: &mut Box<dyn BufRead>,
    regex: &regex::Regex,
    operation: &mut Box<dyn operations::Operation>,
    prepend: &str,
) -> io::Result<()> {
    let mut line = String::new();
    let mut line_no_newline: &str;

    loop {
        let bytes_read = reader.read_line(&mut line)?;
        if bytes_read == 0 {
            break;
        }

        // Strip newline from string used to match against
        line_no_newline = &line[..(bytes_read - 1)];
        let (match_start, match_end) = regex.match_substring(&line_no_newline);

        operation.apply(
            &line_no_newline,
            match_start,
            match_end,
            bytes_read - 1,
            prepend,
        );
        line.clear();
    }
    Ok(())
}

fn get_operation(operation: &str, color: bool) -> io::Result<Box<dyn operations::Operation>> {
    return match operation {
        "p" => Ok(Box::new(operations::PrintMatchingLine { color })),
        "ip" => Ok(Box::new(operations::PrintNonMatchingLine { color })),
        "m" => Ok(Box::new(operations::PrintMatch { color })),
        "im" => Ok(Box::new(operations::PrintExceptMatch { color })),
        "c" => Ok(Box::new(operations::Count { count: 0 })),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Unsupported operation '{}'", operation),
            ))
        }
    };
}

fn is_hidden(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| s.starts_with(".") && s != ".")
        .unwrap_or(false)
}
