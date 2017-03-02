use std;
use unicode_xid::UnicodeXID;
use program::{Program};
use name_table::{NameTable};
use fact_table::{FactTable};
use std::str::{FromStr};

use types::{Term, Literal, Clause};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error<'a> {
    Msg {
        msg: &'static str,
        rest: &'a str,
    }
}

pub type Result<'a, T> = std::result::Result<(T, &'a str), Error<'a>>;

fn err_msg<'a, T>(msg: &'static str, rest: &'a str) -> Result<'a, T> {
    Err(Error::Msg {
        msg: msg,
        rest: rest,
    })
}

fn some_char_is<F>(opt_char: Option<char>, f: F) -> bool where F: Fn(char) -> bool {
    if let Some(c) = opt_char {
        f(c)
    } else {
        false
    }
}

fn substr_index(src: &str, substr: &str) -> usize {
    let diff = substr.as_ptr() as isize - src.as_ptr() as isize;
    if 0 <= diff && diff as usize <= src.len() {
        diff as usize
    } else {
        panic!("substr_index called with invalid substr")
    }
}

fn slice_src<'a>(src: &'a str, rest: &'a str) -> &'a str {
    let index = substr_index(src, rest);
    src.split_at(index).0
}

fn character_is<F>(src: &str, f: F) -> Result<char> where F: Fn(char) -> bool {
    let mut cs = src.chars();
    let c = cs.next();
    if some_char_is(c, f) {
        let rest = cs.as_str();
        return Ok((c.unwrap(), rest));
    } else {
        return err_msg("Wrong character", src);
    }
}

fn character(src: &str, c: char) -> Result<char> {
    character_is(src, |x| c == x)
}

fn start_and_continue<F, G>(src: &str, f: F, g: G) -> Result<&str>
    where F: Fn(char) -> bool,
          G: Fn(char) -> bool {
    let mut rest;
    let mut cs = src.chars();
    if some_char_is(cs.next(), f) {
        rest = cs.as_str();
    } else {
        return err_msg("Wrong starting character", src);
    }
    while some_char_is(cs.next(), &g) {
        rest = cs.as_str();
    }
    return Ok((slice_src(src, rest), rest));
}

fn prefix<'a, 'b>(src: &'a str, prefix: &'b str) -> Result<'a, &'a str> {
    let mut rest = src;
    let mut cs = src.chars();
    let mut ps = prefix.chars();
    while let Some(p) = ps.next() {
        if let Some(c) = cs.next() {
            if p == c {
                rest = cs.as_str();
                continue;
            }
        }
        return err_msg("Prefix did not match", src);
    }
    return Ok((slice_src(src, rest), rest));
}

fn unsigned_decimal_integer(src: &str) -> Result<usize> {
    if let Ok((_, rest)) = character(src, '0') {
        if character_is(rest, |c| c.is_digit(10)).is_ok() {
            err_msg("Octal literal", src)
        } else {
            Ok((0, rest))
        }
    } else {
        let (num_src, rest) = try!(start_and_continue(src, |c| c.is_digit(10), |c| c.is_digit(10)));
        Ok((usize::from_str(num_src).unwrap(), rest))
    }
}

fn char_is_not_uppercase(c: char) -> bool {
    let mut lowered = c.to_lowercase();
    lowered.next() == Some(c) && lowered.next().is_none()
}

fn lowercase_identifier(src: &str) -> Result<&str> {
    start_and_continue(src, |c| UnicodeXID::is_xid_start(c) && char_is_not_uppercase(c),
    UnicodeXID::is_xid_continue)
}

fn uppercase_identifier(src: &str) -> Result<&str> {
    start_and_continue(src, |c| UnicodeXID::is_xid_start(c) && !char_is_not_uppercase(c),
    UnicodeXID::is_xid_continue)
}

fn skip_whitespace(src: &str) -> &str {
    let mut rest = src;
    let mut cs = src.chars();
    loop {
        let c = cs.next();
        if some_char_is(c, char::is_whitespace) {
            rest = cs.as_str();
        } else if c == Some('#') {
            rest = cs.as_str();
            while some_char_is(cs.next(), |c| c != '\n') {
                rest = cs.as_str();
            }
        } else {
            break;
        }
    }
    return rest;
}

fn terms<'a, 'b>(src: &'a str, var_names: &'b mut NameTable) -> Result<'a, Vec<Term>> {
    let mut rest = src;
    let mut result = Vec::new();
    debug!("Looking for terms...");
    loop {
        rest = skip_whitespace(rest);
        if let Ok((var_name, r)) = uppercase_identifier(rest) {
            debug!("found uppercase identifier");
            rest = r;
            let var_index = var_names.get(var_name);
            result.push(Term::Variable(var_index));
        } else if let Ok((number, r)) = unsigned_decimal_integer(rest) {
            debug!("found decimal integer");
            rest = r;
            result.push(Term::Constant(number));
        } else {
            return err_msg("Unexpected character in argument list", rest);
        }
        rest = skip_whitespace(rest);
        if let Ok((_, r)) = character(rest, ',') {
            rest = r;
        } else {
            return Ok((result, rest));
        }
    }
}

fn literal<'a, 'b>(src: &'a str, var_names: &'b mut NameTable, predicate_names: &'b mut NameTable) -> Result<'a, Literal> {
    let mut rest = src;
    rest = skip_whitespace(rest);
    let (predicate_name, r) = try!(lowercase_identifier(rest));
    rest = r;
    rest = skip_whitespace(rest);
    rest = try!(character(rest, '(')).1;
    rest = skip_whitespace(rest);
    let (ts, r) = try!(terms(rest, var_names));
    rest = r;
    rest = skip_whitespace(rest);
    rest = try!(character(rest, ')')).1;
    let predicate = predicate_names.get(predicate_name);
    return Ok((Literal::new_from_vec(predicate, ts), rest));
}

pub fn program(source: &str) -> Result<(FactTable<()>, Program)> {
    let mut rest = source;
    let mut facts = FactTable::new();
    let mut program = Program::new();
    loop {
        rest = skip_whitespace(rest);
        if rest.len() == 0 {
            facts.extend_num_predicates(program.num_predicates());
            return Ok(((facts, program), rest));
        }
        let start_of_clause = rest;
        let mut var_names = NameTable::new();
        let (head, r) = try!(literal(rest, &mut var_names, &mut program.predicate_names)
                             .map(|(lit, r)| (Some(lit), r))
            .or_else(|_| character(rest, '?').map(|(_, r)| (None, r))));
        rest = r;
        rest = skip_whitespace(rest);
        if let Ok((_, r)) = prefix(rest, ":-") {
            rest = r;
            let mut literals = Vec::new();
            rest = skip_whitespace(rest);
            loop {
                let (lit, r) = try!(literal(rest, &mut var_names, &mut program.predicate_names));
                rest = r;
                literals.push(lit);
                rest = skip_whitespace(rest);
                if let Ok((_, r)) = character(rest, ',') {
                    rest = r;
                } else {
                    break;
                }
            }
            let clause = Clause {
                head: head,
                body: literals,
            };
            if clause.is_valid() {
                program.clause_variable_names.insert(program.clauses.len(), var_names);
                program.clauses.push(clause);
            } else {
                return err_msg("Invalid clause", start_of_clause);
            }
        } else if let (Some(lit), Ok((_, r))) = (head, character(rest, '.')) {
            rest = r;
            if var_names.to_reverse().len() != 0 {
                return err_msg("Fact contained variables", start_of_clause);
            }
            facts.add_fact(lit.to_fact(), ());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{unsigned_decimal_integer, character_is, lowercase_identifier, uppercase_identifier,
    terms, prefix, literal, program};
    use types::{Term, Literal};
    use name_table::{NameTable};

    #[test]
    fn parse_characters() {
        assert!(character_is("1", |c| c.is_digit(10)).is_ok());
    }

    #[test]
    fn parse_unsigned_decimal_integer() {
        assert!(unsigned_decimal_integer("").is_err());
        assert!(unsigned_decimal_integer("0").is_ok());
        assert!(unsigned_decimal_integer("1").is_ok());
        assert!(unsigned_decimal_integer("10").is_ok());
        assert!(unsigned_decimal_integer("100").is_ok());
        assert!(unsigned_decimal_integer("01").is_err());
        assert!(unsigned_decimal_integer("1234567890").is_ok());
    }

    #[test]
    fn parse_lowercase_identifier() {
        assert!(lowercase_identifier("").is_err());
        assert!(lowercase_identifier("A").is_err());
        assert!(lowercase_identifier("0").is_err());
        assert!(lowercase_identifier("1").is_err());
        assert!(lowercase_identifier("a").is_ok());
        assert_eq!(lowercase_identifier("aB").unwrap().0, "aB");
        assert_eq!(lowercase_identifier("a1").unwrap().0, "a1");
        assert!(lowercase_identifier("_a").is_err());
        assert_eq!(lowercase_identifier("test_identifier").unwrap().0, "test_identifier");
        assert_eq!(lowercase_identifier("test_identifier").unwrap().1, "");
    }

    #[test]
    fn parse_uppercase_identifier() {
        assert!(uppercase_identifier("").is_err());
        assert!(uppercase_identifier("A").is_ok());
        assert!(uppercase_identifier("0").is_err());
        assert!(uppercase_identifier("1").is_err());
        assert!(uppercase_identifier("a").is_err());
        assert!(uppercase_identifier("aB").is_err());
        assert!(uppercase_identifier("Ba").is_ok());
        assert_eq!(uppercase_identifier("Test_identifier").unwrap().0, "Test_identifier");
        assert_eq!(uppercase_identifier("Test_identifier").unwrap().1, "");
    }

    #[test]
    fn parse_terms() {
        let mut var_names = NameTable::new();
        assert!(terms("0", &mut var_names).is_ok());
        assert!(terms("0, 1", &mut var_names).is_ok());
        assert!(terms("0 ,1", &mut var_names).is_ok());
        assert!(terms("0 , 1", &mut var_names).is_ok());
        assert!(terms("X", &mut var_names).is_ok());
        assert_eq!(terms("X", &mut var_names).unwrap().1, "");
        assert!(terms("Y", &mut var_names).is_ok());
        assert_eq!(var_names.get("Y"), 1);
        assert!(terms("100", &mut var_names).is_ok());
        assert!(terms("0 , X, 100", &mut var_names).is_ok());
        assert!(terms("x", &mut var_names).is_err());
        assert!(terms(",", &mut var_names).is_err());
        assert!(terms("", &mut var_names).is_err());
    }

    #[test]
    fn test_prefix() {
        assert!(prefix("test", "te").is_ok());
        assert_eq!(prefix("test", "te").unwrap().1, "st");
    }

    #[test]
    fn test_literal() {
        let mut var_names = NameTable::new();
        let mut pred_names = NameTable::new();
        assert!(literal("a(0)", &mut var_names, &mut pred_names).is_ok());
        assert!(literal("b(0, 1, X, Y)", &mut var_names, &mut pred_names).is_ok());
        assert_eq!(var_names.get("Y"), 1);
        assert_eq!(pred_names.get("b"), 1);
        assert_eq!(literal("b(1, X, Y)", &mut var_names, &mut pred_names).unwrap().0,
        Literal::new_from_vec(1, vec![Term::Constant(1), Term::Variable(0), Term::Variable(1)]));
    }

    #[test]
    fn test_program() {
        assert!(program("a(0).").is_ok());
        assert!(program("a(X) :- b(X)").is_ok());
        assert!(program("a(X) :- b(X), c(Y)").is_ok());
        assert!(program("a(X) :- b(X, Y), c(Y), test(Y)").is_ok());
        assert!(program("#A comment").is_ok());
        assert!(program("#A comment\nA(0).").is_err());
    }
}
