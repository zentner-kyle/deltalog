use std;
use unicode_xid::UnicodeXID;
use program::{Program};
use name_table::{NameTable};
use fact_table::{FactTable};
use std::str::{FromStr};
use truth_value::{TruthValue};
use std::collections::hash_map::{HashMap, Entry};

use types::{Term, Literal, Clause, Constant, Fact, Predicate};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error<'a> {
    Msg {
        msg: &'static str,
        rest: &'a str,
    }
}

pub type Result<'a, T> = std::result::Result<(T, &'a str), Error<'a>>;

fn err_msg<'a, T>(msg: &'static str, rest: &'a str) -> Result<'a, T> {
    Err(err_from_str(msg, rest))
}

fn err_from_str<'a>(msg: &'static str, rest: &'a str) -> Error<'a> {
    Error::Msg {
        msg: msg,
        rest: rest,
    }
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
        let (num_src, rest) = start_and_continue(src, |c| c.is_digit(10), |c| c.is_digit(10))?;
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

fn literal<'a, 'b, 'c>(src: &'a str, var_names: &'b mut NameTable, predicate_names: &'b mut NameTable, num_terms: &'c mut HashMap<Predicate, usize>) -> Result<'a, Literal> {
    let mut rest = src;
    rest = skip_whitespace(rest);
    let (predicate_name, r) = lowercase_identifier(rest)?;
    rest = r;
    rest = skip_whitespace(rest);
    rest = character(rest, '(')?.1;
    rest = skip_whitespace(rest);
    let (ts, r) = terms(rest, var_names)?;
    rest = r;
    rest = skip_whitespace(rest);
    rest = character(rest, ')')?.1;
    let predicate = predicate_names.get(predicate_name);
    if correct_num_terms(num_terms, predicate, ts.len()) {
        return Ok((Literal::new_from_vec(predicate, ts), rest));
    } else {
        return err_msg("Wrong number of terms in literal", src)
    }
}

fn correct_num_terms<'a>(num_terms: &'a mut HashMap<Predicate, usize>, predicate: Predicate, new_num_terms: usize) -> bool {
    match num_terms.entry(predicate) {
        Entry::Occupied(pair) => {
            *pair.get() == new_num_terms
        },
        Entry::Vacant(pair) => {
            pair.insert(new_num_terms);
            true
        }
    }
}

fn fact_terms<'a, 'b>(src: &'a str) -> Result<'a, Vec<Constant>> {
    let mut rest = src;
    let mut result = Vec::new();
    loop {
        rest = skip_whitespace(rest);
        if let Ok((number, r)) = unsigned_decimal_integer(rest) {
            debug!("found decimal integer");
            rest = r;
            result.push(number);
        } else {
            return err_msg("Unexpected character in fact argument list", rest);
        }
        rest = skip_whitespace(rest);
        if let Ok((_, r)) = character(rest, ',') {
            rest = r;
        } else {
            return Ok((result, rest));
        }
    }
}


fn fact<'a, 'b>(src: &'a str, predicate_names: &'b mut NameTable) -> Result<'a, Fact> {
    let mut rest = src;
    rest = skip_whitespace(rest);
    let (predicate_name, r) = lowercase_identifier(rest)?;
    rest = r;
    rest = skip_whitespace(rest);
    rest = character(rest, '(')?.1;
    rest = skip_whitespace(rest);
    let (ts, r) = fact_terms(rest)?;
    rest = r;
    rest = skip_whitespace(rest);
    rest = character(rest, ')')?.1;
    let predicate = predicate_names.get(predicate_name);
    return Ok((Fact::new_from_vec(predicate, ts), rest));
}

fn fact_list<'a, 'b, T>(src: &'a str, predicate_names: &'b mut NameTable) -> Result<'a, FactTable<T>> where T: TruthValue {
    let mut rest = src;
    let mut results = FactTable::new();
    loop {
        rest = skip_whitespace(rest);
        if let Ok((fact, r)) = fact(rest, predicate_names) {
            rest = r;
            results.set(fact, T::default());
        } else {
            return err_msg("Unexpected character in fact list", rest);
        }
        rest = skip_whitespace(rest);
        if let Ok((_, r)) = character(rest, ',') {
            rest = r;
        } else {
            return Ok((results, rest));
        }
    }
}

pub fn weight<T>(source: &str) -> Result<T::Dual> where T: TruthValue {
    let mut rest = source;
    rest = prefix(rest, "weight")?.1;
    rest = skip_whitespace(rest);
    rest = character(rest, '(')?.1;
    rest = skip_whitespace(rest);
    if let Some((truth, r)) = T::parse_dual(rest) {
        rest = r;
        rest = skip_whitespace(rest);
        rest = character(rest, ')')?.1;
        return Ok((truth, rest));
    } else {
        return err_msg("Invalid weight", rest);
    }
}

pub fn confidence<T>(source: &str) -> Result<T> where T: TruthValue {
    let mut rest = source;
    rest = prefix(rest, "confidence")?.1;
    rest = skip_whitespace(rest);
    rest = character(rest, '(')?.1;
    rest = skip_whitespace(rest);
    if let Some((truth, r)) = T::parse(rest) {
        rest = r;
        rest = skip_whitespace(rest);
        rest = character(rest, ')')?.1;
        return Ok((truth, rest));
    } else {
        return err_msg("Invalid confidence", rest);
    }
}

pub fn sample<'a, 'b, T>(source: &'a str, predicate_names: &'b mut NameTable) -> Result<'a, (FactTable<T>, FactTable<T>)> where T: TruthValue {
    let mut rest = source;
    rest = prefix(rest, "sample")?.1;
    rest = skip_whitespace(rest);
    let (input, r) = fact_list(rest, predicate_names)?;
    rest = r;
    rest = skip_whitespace(rest);
    rest = prefix(rest, "output")?.1;
    rest = skip_whitespace(rest);
    let (output, r) = fact_list(rest, predicate_names)?;
    rest = r;
    rest = skip_whitespace(rest);
    rest = character(rest, '.')?.1;
    return Ok(((input, output), rest));
}


pub fn program<T>(source: &str) -> Result<(FactTable<T>, Program<T>, Vec<(FactTable<T>, FactTable<T>)>)> where T: TruthValue {
    let mut rest = source;
    let mut facts = FactTable::new();
    let mut program = Program::new();
    let mut current_weight = T::dual_default();
    let mut current_confidence = T::default();
    let mut samples: Vec<(FactTable<T>, FactTable<T>)> = Vec::new();
    loop {
        rest = skip_whitespace(rest);
        if rest.len() == 0 {
            facts.extend_num_predicates(program.num_predicates());
            return Ok(((facts, program, samples), rest));
        }
        let start_of_clause = rest;
        if let Ok((sample, r)) = sample(rest, &mut program.predicate_names) {
            program.check_num_fact_terms(&sample.0).map_err(|s| err_from_str(s, rest))?;
            program.check_num_fact_terms(&sample.1).map_err(|s| err_from_str(s, rest))?;
            rest = r;
            samples.push(sample);
            continue;
        }
        if let Ok((weight, r)) = weight::<T>(rest) {
            rest = r;
            rest = skip_whitespace(rest);
            current_weight = weight;
        }
        if let Ok((confidence, r)) = confidence(rest) {
            rest = r;
            rest = skip_whitespace(rest);
            current_confidence = confidence;
        }
        let mut var_names = NameTable::new();
        let (head, r) = literal(rest, &mut var_names, &mut program.predicate_names, &mut program.predicate_num_terms)
            .map(|(lit, r)| (Some(lit), r))
            .or_else(|_| character(rest, '?').map(|(_, r)| (None, r)))?;
        rest = r;
        rest = skip_whitespace(rest);
        if let Ok((_, r)) = prefix(rest, ":-") {
            rest = r;
            let mut literals = Vec::new();
            rest = skip_whitespace(rest);
            loop {
                let (lit, r) = literal(rest, &mut var_names, &mut program.predicate_names, &mut program.predicate_num_terms)?;
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
                program.clause_weights.push(current_weight.clone());
            } else {
                return err_msg("Invalid clause", start_of_clause);
            }
        } else if let (Some(lit), Ok((_, r))) = (head, character(rest, '.')) {
            if var_names.to_reverse().len() != 0 {
                return err_msg("Fact contained variables", start_of_clause);
            }
            let fact = lit.to_fact();
            program.check_num_single_fact_terms(&fact).map_err(|s| err_from_str(s, rest))?;
            facts.add_fact(fact, current_confidence.clone());
            rest = r;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{unsigned_decimal_integer, character_is, lowercase_identifier, uppercase_identifier,
    terms, prefix, literal, program};
    use types::{Term, Literal};
    use name_table::{NameTable};
    use std::collections::hash_map::{HashMap};

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
        let mut num_terms = HashMap::new();
        assert!(literal("a(0)", &mut var_names, &mut pred_names, &mut num_terms).is_ok());
        assert!(literal("b(0, 1, X, Y)", &mut var_names, &mut pred_names, &mut num_terms).is_ok());
        assert_eq!(var_names.get("Y"), 1);
        assert_eq!(pred_names.get("b"), 1);
        assert_eq!(literal("c(1, X, Y)", &mut var_names, &mut pred_names, &mut num_terms).unwrap().0,
        Literal::new_from_vec(2, vec![Term::Constant(1), Term::Variable(0), Term::Variable(1)]));
    }

    #[test]
    fn test_program() {
        assert!(program::<()>("a(0).").is_ok());
        assert!(program::<()>("a(X) :- b(X)").is_ok());
        assert!(program::<()>("a(X) :- b(X), c(Y)").is_ok());
        assert!(program::<()>("a(X) :- b(X, Y), c(Y), test(Y)").is_ok());
        assert!(program::<()>("#A comment").is_ok());
        assert!(program::<()>("#A comment\nA(0).").is_err());
    }
}
