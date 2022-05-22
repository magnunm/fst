/// The smallest range of byte indices such that outside the range
/// are purely literal strings.
pub fn non_literal_range(regex: &str) -> (usize, usize) {
    let start_index_non_literal = first_non_literal_regex_char(regex);

    if start_index_non_literal == regex.len() {
        return (start_index_non_literal, start_index_non_literal);
    }

    // NOTE: this will give a non-literal range which has the end one further than needed in the
    // case of operators. This because the operators work on the char to the left of themselves,
    // not to the right.
    let end_index_non_literal: usize =
        regex.len() - first_non_literal_regex_char(&regex.chars().rev().collect::<String>());
    return (start_index_non_literal, end_index_non_literal);
}

fn first_non_literal_regex_char(regex: &str) -> usize {
    // Finding an alteration before any grouping means any potential literal string head found this
    // far is not valid. This since it is alterated with some other expression which might not
    // contain the literal string.
    if let Some(first_alteration) = find_predicate(regex, |c| c == '|') {
        let first_grouping = find_predicate(regex, is_regex_grouping);
        if first_grouping.is_none() || first_grouping.unwrap() > first_alteration {
            return 0;
        }
    }

    let mut regex_char_indices = regex.char_indices();
    let mut index_non_literal = 0;
    let mut previous_char_size_bytes: usize = 0;

    while let Some((char_byte_index, character)) = regex_char_indices.next() {
        index_non_literal = char_byte_index;

        if not_part_of_regex_literal(character) {
            if is_regex_operator(character) {
                // These work on a single character, excluding
                // alterations which are handled separately.
                return index_non_literal - previous_char_size_bytes;
            }

            return index_non_literal;
        }

        previous_char_size_bytes = character.len_utf8();
    }

    // Done with the loop because we reached the end of the regex characters without detecting a
    // single non-literal char. Then the last char was also literal, and we need to add its length.
    return index_non_literal + previous_char_size_bytes;
}

fn not_part_of_regex_literal(c: char) -> bool {
    ['\\', '|'].contains(&c)
        || is_regex_grouping(c)
        || is_regex_operator(c)
        || is_regex_character_class(c)
}

fn is_regex_operator(c: char) -> bool {
    ['+', '?', '*', '|'].contains(&c)
}

fn is_regex_character_class(c: char) -> bool {
    ['[', ']', '.'].contains(&c)
}

fn is_regex_grouping(c: char) -> bool {
    ['(', ')'].contains(&c)
}

/// Byte index of first character matching the predicate.
fn find_predicate(in_str: &str, predicate: fn(char) -> bool) -> Option<usize> {
    in_str
        .char_indices()
        .filter(|&(_, c)| predicate(c))
        .map(|(i, _)| i)
        .next()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_non_literal_range() -> Result<(), &'static str> {
        assert_eq!(non_literal_range("abc?"), (2, 4)); // == "c?"
        assert_eq!(non_literal_range("abc|def"), (0, "abc|def".len()));
        assert_eq!(non_literal_range("(abc)def"), (0, "(abc)".len()));

        // NOTE: In these operator cases gives the end of the non-literal range as one to much.
        // Limitation of the implementation that has no impact on overall correctness.
        assert_eq!(non_literal_range("abc?de"), (2, 5)); // == "c?d"

        Ok(())
    }

    #[test]
    fn test_first_non_literal_char() -> Result<(), &'static str> {
        // First non-literal is `c` since it is operated on by `?`.
        assert_eq!(first_non_literal_regex_char("abc?"), 2);
        assert_eq!(first_non_literal_regex_char("abc?def"), 2);

        assert_eq!(first_non_literal_regex_char("abc"), 3);
        assert_eq!(first_non_literal_regex_char("foo|bar"), 0);
        assert_eq!(first_non_literal_regex_char("ab*|x"), 0);
        assert_eq!(first_non_literal_regex_char("ab|x+"), 0);
        assert_eq!(first_non_literal_regex_char("a(b*|x)"), 1);
        assert_eq!(first_non_literal_regex_char("a|(b*x)"), 0);

        Ok(())
    }
}
