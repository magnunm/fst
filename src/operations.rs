use ansi_term::Colour::{Blue, Red};

/// An operation to be applied to the lines of the input file. Given the line together with the
/// start and end of the substring matching the regex pattern.
pub trait Operation {
    fn apply(
        &mut self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        prepend: &str,
    );

    /// For operations that have something to "report" after processing all the lines. Like the
    /// count operation.
    fn final_report(&self) -> String;
}

pub struct PrintMatchingLine {
    pub color: bool,
}

impl Operation for PrintMatchingLine {
    /// Print the given line if it contains a nonempty matching substring.
    fn apply(
        &mut self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        prepend: &str,
    ) {
        if match_start == match_end {
            return;
        }

        if self.color {
            print!("{}", Blue.paint(prepend));
        } else {
            print!("{}", prepend);
        }

        if self.color {
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

    fn final_report(&self) -> String {
        String::new()
    }
}

pub struct PrintNonMatchingLine {
    pub color: bool,
}

impl Operation for PrintNonMatchingLine {
    /// Print the given line if it does not contain a nonempty matching substring.
    fn apply(
        &mut self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        prepend: &str,
    ) {
        if match_start == match_end {
            if self.color {
                print!("{}", Blue.paint(prepend));
            } else {
                print!("{}", prepend);
            }
            println!("{}", &line[..line_length])
        }
    }

    fn final_report(&self) -> String {
        String::new()
    }
}

pub struct PrintMatch {
    pub color: bool,
}

impl Operation for PrintMatch {
    /// Print the mathcing substring of a matching line.
    fn apply(
        &mut self,
        line: &str,
        match_start: usize,
        match_end: usize,
        _line_length: usize,
        prepend: &str,
    ) {
        if match_start == match_end {
            return;
        }
        if self.color {
            print!("{}", Blue.paint(prepend));
        } else {
            print!("{}", prepend);
        }
        println!("{}", &line[match_start..match_end])
    }

    fn final_report(&self) -> String {
        String::new()
    }
}

pub struct PrintExceptMatch {
    pub color: bool,
}

impl Operation for PrintExceptMatch {
    /// Print all but the mathcing substring of a matching line.
    fn apply(
        &mut self,
        line: &str,
        match_start: usize,
        match_end: usize,
        line_length: usize,
        prepend: &str,
    ) {
        if match_start == match_end {
            return;
        }

        if self.color {
            print!("{}", Blue.paint(prepend));
        } else {
            print!("{}", prepend);
        }

        println!("{}{}", &line[..match_start], &line[match_end..line_length]);
    }

    fn final_report(&self) -> String {
        String::new()
    }
}

pub struct Count {
    pub count: usize,
}

impl Operation for Count {
    fn apply(
        &mut self,
        _line: &str,
        match_start: usize,
        match_end: usize,
        _line_length: usize,
        _prepend: &str,
    ) {
        if match_start != match_end {
            self.count += 1
        }
    }

    fn final_report(&self) -> String {
        self.count.to_string()
    }
}
