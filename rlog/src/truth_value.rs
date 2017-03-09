use std::fmt::{Debug};

pub trait TruthValue : Clone + PartialEq + Debug {
    type Dual: Clone + PartialEq + Debug;
    fn default() -> Self;
    fn dual_default() -> Self::Dual;
    fn parse(&str) -> Option<(Self, &str)>;
    fn parse_dual(&str) -> Option<(Self::Dual, &str)>;
    fn merge(a: &Self, b: &Self) -> Self;
    fn finalize(dual: &Self::Dual, a: &Self) -> Self;
    fn join(a: &Self, b: &Self) -> Self;
    fn as_datalog(&self) -> String;
    fn dual_as_datalog(dual: &Self::Dual) -> String;
}

impl TruthValue for () {
    type Dual = ();
    fn default() -> Self {}
    fn dual_default() -> Self::Dual {}
    fn join(_: &Self, _: &Self) -> Self {}
    fn merge(_: &Self, _: &Self) -> Self {}
    fn finalize(_: &Self::Dual, _: &Self) -> Self {}
    fn parse(_: &str) -> Option<(Self, &str)> { None }
    fn parse_dual(_: &str) -> Option<(Self::Dual, &str)> { None }
    fn as_datalog(&self) -> String {
        return "".to_owned();
    }
    fn dual_as_datalog(_: &Self::Dual) -> String {
        return "".to_owned();
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct MaxFloat64(f64);

impl TruthValue for MaxFloat64 {
    type Dual = f64;

    fn default() -> Self {
        MaxFloat64(1.0) 
    }

    fn dual_default() -> Self::Dual {
        1.0
    }

    fn join(a: &Self, b: &Self) -> Self {
        MaxFloat64(f64::max(a.0, b.0))
    }

    fn merge(a: &Self, b: &Self) -> Self {
        MaxFloat64(f64::min(a.0, b.0))
    }

    fn finalize(dual: &Self::Dual, a: &Self) -> Self {
        MaxFloat64(dual * a.0)
    }

    fn parse(src: &str) -> Option<(Self, &str)> {
        Self::parse_dual(src).map(|(f, rest)| (MaxFloat64(f), rest))
    }

    fn parse_dual(src: &str) -> Option<(Self::Dual, &str)> {
        if let Some(end) = src.find(')') {
            use std::str::FromStr;
            let (f_src, rest) = src.split_at(end);
            if let Ok(f) = f64::from_str(f_src) {
                return Some((f, rest));
            }
        }
        return None;
    }

    fn as_datalog(&self) -> String {
        return format!("confidence({}) ", self.0);
    }

    fn dual_as_datalog(dual: &Self::Dual) -> String {
        return format!("weight({}) ", dual);
    }
}
