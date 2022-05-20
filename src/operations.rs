use ansi_term::Colour::{Blue, Red};

/// An operation to be applies to the lines of the input file. Given the line together with the
/// start and end of the substring matching the regex pattern.
pub trait Operation {
    fn apply(
        &self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        color: bool,
        prepend: &str,
    );
}

pub struct PrintMatchingLine;

impl Operation for PrintMatchingLine {
    /// Print the given line if it contains a nonempty matching substring.
    fn apply(
        &self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        color: bool,
        prepend: &str,
    ) {
        if match_start == match_end {
            return;
        }

        if color {
            print!("{}", Blue.paint(prepend));
        } else {
            print!("{}", prepend);
        }

        if color {
            println!(
                "{}{}{}",
                &line[..match_start],
                Red.paint(&line[match_start..match_end]),
                &line[match_end..line_length]
            );
            return;
        }
        println!("{}", &line[..line_length])
    }
}

pub struct PrintNonMatchingLine;

impl Operation for PrintNonMatchingLine {
    /// Print the given line if it does not contain a nonempty matching substring.
    fn apply(
        &self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        color: bool,
        prepend: &str,
    ) {
        if match_start == match_end {
            if color {
                print!("{}", Blue.paint(prepend));
            } else {
                print!("{}", prepend);
            }
            println!("{}", &line[..line_length])
        }
    }
}

pub struct PrintMatch;

impl Operation for PrintMatch {
    /// Print the mathcing substring of a matching line.
    fn apply(
        &self,
        line: &str,
        match_start: usize,
        match_end: usize,
        _line_length: usize,
        color: bool,
        prepend: &str,
    ) {
        if match_start == match_end {
            return;
        }
        if color {
            print!("{}", Blue.paint(prepend));
        } else {
            print!("{}", prepend);
        }
        println!("{}", &line[match_start..match_end])
    }
}

pub struct PrintExceptMatch;

impl Operation for PrintExceptMatch {
    /// Print all but the mathcing substring of a matching line.
    fn apply(
        &self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        color: bool,
        prepend: &str,
    ) {
        if match_start == match_end {
            return;
        }

        if color {
            print!("{}", Blue.paint(prepend));
        } else {
            print!("{}", prepend);
        }

        println!("{}{}", &line[..match_start], &line[match_end..line_length]);
    }
}

pub struct Count;

impl Operation for Count {
    fn apply(
        &self,
        _line: &str,
        _match_start: usize,
        _match_end: usize,
        _line_length: usize,
        _color: bool,
        _prepend: &str,
    ) {
    }
}
