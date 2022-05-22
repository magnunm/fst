mod builder;
pub mod machine;

use std::vec::Vec;

use builder::NFABuilder;
use machine::{StateRegister, NFA};

/// Convert a regular expression to a NFA.
///
/// Uses the Shunting-Yard algorithm to convert a regex written in
/// infix notation to a postifix regex. Then applies the Thompson
/// construction to that postfix regex in order to convert it into a
/// NFA. The two algorithms are combined so that this is done in a
/// single loop over the regex characters. We don't use explicit
/// concatenation characters in the infix notation, while the postfix
/// notation requires it. This requires some logic to decide
/// where the "would be" a concatenation character in the postifix
/// notation.
pub fn regex_to_nfa<'a>(regex: &'a str) -> Result<NFA<'a>, &'static str> {
    check_for_invalid(regex)?;

    let mut nfa_builder = NFABuilder {
        register: StateRegister::new(),
        fragment_stack: Vec::new(),
        operator_stack: Vec::new(),
    };

    let mut previous_char: Option<char> = None;
    let mut regex_char_indices = regex.char_indices();

    while let Some((char_byte_index, character)) = regex_char_indices.next() {
        // Determine if the current character should be concatenated
        // with the previous. If so we temporarily act as if we were
        // looking at a concatenation character (~). Since in infix
        // notation this operator would be between the two characters
        // this logic needs to go first.
        let mut concatenate_previous: bool = false;

        if let Some(previous_char_inner) = previous_char {
            concatenate_previous = are_concatenated(previous_char_inner, character);
        }

        if concatenate_previous {
            nfa_builder.handle_operator('~')?;
        }

        // The escape character. Treat the next character as literal no
        // matter what.
        if character == '\\' {
            if let Some((_, next_character)) = regex_char_indices.next() {
                nfa_builder.parse_literal_to_nfa(next_character);
            } else {
                return Err("Regex cannot end in a escape");
            }

            // To handle concatenation correctly on the next iteration the
            // previous character must be recongized as a literal char.
            // The actual value does not matter.
            previous_char = Some('a');
            continue;
        }

        // A bracket character class, whose NFA state stores a slice
        // into the regex.
        if character == '[' {
            let mut end_bracket_char_byte_index: Option<usize> = None;
            while let Some((i, character)) = regex_char_indices.next() {
                if character == ']' {
                    end_bracket_char_byte_index = Some(i);
                    break;
                }
            }

            if end_bracket_char_byte_index.is_none() {
                return Err("End of regex before end of bracket");
            }

            nfa_builder.parse_bracket_character_class_to_nfa(
                &regex[(char_byte_index + 1)..end_bracket_char_byte_index.unwrap()],
            );

            previous_char = Some(']');
            continue;
        }

        // Handle the current character
        match character {
            // Operators. Exluding concatenation which is not treated as
            // an operator in the infix notation so should match a literal
            // character here.
            '*' | '+' | '?' | '|' => {
                nfa_builder.handle_operator(character)?;
            }
            // Parentheses: grouping
            '(' => {
                nfa_builder.operator_stack.push('(');
            }
            ')' => {
                nfa_builder.handle_closing_paren()?;
            }
            '.' => {
                nfa_builder.parse_dot_character_class_to_nfa();
            }
            // Default: Literal character
            _ => {
                nfa_builder.parse_literal_to_nfa(character);
            }
        }

        previous_char = Some(character);
    }

    nfa_builder.empty_operator_stack()?;

    let final_nfa_fragment = nfa_builder.finalize()?;

    Ok(NFA {
        state_register: nfa_builder.register,
        start_state: final_nfa_fragment.start,
    })
}

fn check_for_invalid(regex: &str) -> Result<(), &'static str> {
    if regex.is_empty() {
        return Err("Empty regex pattern not supported");
    }
    // TODO: Add more checks.
    Ok(())
}

/// Determine if two characters are concatenated if they appear after
/// each other in a infix regex.
fn are_concatenated(character: char, next_character: char) -> bool {
    !(['*', '+', '?', '|', ')'].contains(&next_character) || ['|', '('].contains(&character))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_nfa_matching_1() -> Result<(), &'static str> {
        // Test alteration, unicode, concatenation
        let regex: &str = "(a|⻘)c";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ac", false), Some(2));
        assert_eq!(nfa.simulate("⻘c", false), Some("⻘c".len()));
        assert_eq!(nfa.simulate("a", false), None); // Missing c
        assert_eq!(nfa.simulate("c", false), None); // Missing first char
        assert_eq!(nfa.simulate("xc", false), None); // Wrong first char
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_2() -> Result<(), &'static str> {
        // Test one or more, zero or more
        let regex: &str = "(a|b)*c+";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ac", false), Some(2));
        assert_eq!(nfa.simulate("c", false), Some(1));
        assert_eq!(nfa.simulate("aaaac", false), Some(5));
        assert_eq!(nfa.simulate("accccc", false), Some(2)); // 2 not greedy
        assert_eq!(nfa.simulate("bc", false), Some(2));
        assert_eq!(nfa.simulate("abc", false), Some(3)); // Both characters allowed in zero or more
        assert_eq!(nfa.simulate("b", false), None); // Too few c
        assert_eq!(nfa.simulate("", false), None); // Too few c
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_3() -> Result<(), &'static str> {
        // Test character classes
        let regex: &str = ".*a[0-9]+"; // Any string that ends in `a` + number
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a2021", true), Some(5));
        assert_eq!(nfa.simulate("åa9", true), Some("åa9".len()));
        assert_eq!(nfa.simulate("教育漢字a0", true), Some("教育漢字a0".len()));
        assert_eq!(nfa.simulate("b", true), None);
        assert_eq!(nfa.simulate("a", true), None);
        assert_eq!(nfa.simulate("aO", true), None);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_4() -> Result<(), &'static str> {
        // Test escaping
        let regex: &str = r"(\.\*)+"; // One or more literal .*
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate(".*.*.*.*", true), Some(8));
        assert_eq!(nfa.simulate(".*.*.*.*", false), Some(2));
        assert_eq!(nfa.simulate(".*", false), Some(2));
        assert_eq!(nfa.simulate(".", false), None);
        assert_eq!(nfa.simulate("a", false), None);
        assert_eq!(nfa.simulate("\\.\\*", false), None);
        assert_eq!(nfa.simulate("\\.", false), None);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_5() -> Result<(), &'static str> {
        // Test or and concatenation
        let regex: &str = "a(b|c)";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("ab", false), Some(2));
        assert_eq!(nfa.simulate("ac", false), Some(2));
        assert_eq!(nfa.simulate("abx", false), Some(2));
        assert_eq!(nfa.simulate("a", false), None);
        assert_eq!(nfa.simulate("aa", false), None);
        assert_eq!(nfa.simulate("", false), None);
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_6() -> Result<(), &'static str> {
        // Test zero or more, one or more, zero or one
        // Test unicode support
        let regex: &str = "a*ø?⻘+";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a⻘", false), Some("a⻘".len()));
        assert_eq!(nfa.simulate("aø⻘", false), Some("aø⻘".len()));
        assert_eq!(nfa.simulate("⻘", false), Some("⻘".len()));
        assert_eq!(nfa.simulate("ø⻘", false), Some("ø⻘".len()));
        assert_eq!(
            nfa.simulate("aaaaaaaaaaaa⻘⻘⻘⻘⻘⻘", false),
            Some("aaaaaaaaaaaa⻘".len())
        );
        assert_eq!(nfa.simulate("aøø⻘", false), None); // Too many ø
        assert_eq!(nfa.simulate("aø", false), None); // Too few ⻘
        Ok(())
    }

    #[test]
    fn test_regex_nfa_matching_7() -> Result<(), &'static str> {
        // Test detection of a match on the empty string.
        let regex: &str = "a*";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("a", false), Some(0));
        assert_eq!(nfa.simulate("a", true), Some(1));
        assert_eq!(nfa.simulate("", false), Some(0));
        assert_eq!(nfa.simulate("", true), Some(0));
        assert_eq!(nfa.simulate("b", false), Some(0));
        assert_eq!(nfa.simulate("b", true), Some(0));
        Ok(())
    }

    // Test that grouping without applying any operation on the group gives the same results as if
    // the group was not there.
    #[test]
    fn test_grouping_with_no_operator() -> Result<(), &'static str> {
        let regex: &str = "x(y)z";
        let nfa = regex_to_nfa(regex)?;

        assert_eq!(nfa.simulate("xy", true), None);
        assert_eq!(nfa.simulate("xz", true), None);
        assert_eq!(nfa.simulate("xyz", true), Some(3));
        Ok(())
    }

    /// Test greedy vs non-greedy matching
    #[test]
    fn test_greedyness() -> Result<(), &'static str> {
        let mut nfa = regex_to_nfa("a+")?;
        assert_eq!(nfa.simulate("aaa", true), Some(3));
        assert_eq!(nfa.simulate("aaa", false), Some(1));

        nfa = regex_to_nfa("a*")?;
        assert_eq!(nfa.simulate("aaa", true), Some(3));
        assert_eq!(nfa.simulate("aaa", false), Some(0));

        nfa = regex_to_nfa("[0-9]+")?;
        assert_eq!(nfa.simulate("2002", true), Some("2002".len()));
        assert_eq!(nfa.simulate("2002", false), Some(1));

        Ok(())
    }
}
