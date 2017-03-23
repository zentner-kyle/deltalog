use std::fmt::{Debug};
use std::cmp::{Ordering};

pub trait TruthValue : Clone + PartialEq + Eq + Debug + PartialOrd + Ord {
    type Dual: Clone + PartialEq + Debug + PartialOrd + Ord;
    fn default() -> Self;
    fn zero() -> Self;
    fn dual_default() -> Self::Dual;
    fn parse(&str) -> Option<(Self, &str)>;
    fn parse_dual(&str) -> Option<(Self::Dual, &str)>;
    fn either(a: &Self, b: &Self) -> Self;
    fn back_either(a: &Self, b: &Self, err: &Self) -> (Self, Self);
    fn both(a: &Self, b: &Self) -> Self;
    fn back_both(a: &Self, b: &Self, err: &Self) -> (Self, Self);
    fn finalize(dual: &Self::Dual, a: &Self) -> Self;
    fn back_finalize(dual: &Self::Dual, a: &Self, err: &Self) -> Self;
    fn sub(a: &Self, b: &Self) -> Self;
    fn sum(a: &Self, b: &Self) -> Self;
    fn dual_adjust(a: &Self::Dual, b: &Self) -> Self::Dual;
    fn as_datalog(&self) -> String;
    fn dual_as_datalog(dual: &Self::Dual) -> String;
}

impl TruthValue for () {
    type Dual = ();
    fn default() -> Self {}
    fn zero() -> Self {}
    fn dual_default() -> Self::Dual {}
    fn either(_: &Self, _: &Self) -> Self {}
    fn back_either(_: &Self, _: &Self, _: &Self) -> (Self, Self) { ((), ()) }
    fn both(_: &Self, _: &Self) -> Self {}
    fn back_both(_: &Self, _: &Self, _: &Self) -> (Self, Self) { ((), ()) }
    fn finalize(_: &Self::Dual, _: &Self) -> Self {}
    fn back_finalize(_: &Self::Dual, _: &Self, _: &Self) -> Self {}
    fn sub(_: &Self, _: &Self) -> Self {}
    fn sum(_: &Self, _: &Self) -> Self {}
    fn dual_adjust(_: &Self::Dual, _: &Self) -> Self::Dual {}
    fn parse(_: &str) -> Option<(Self, &str)> { None }
    fn parse_dual(_: &str) -> Option<(Self::Dual, &str)> { None }
    fn as_datalog(&self) -> String {
        return "".to_owned();
    }
    fn dual_as_datalog(_: &Self::Dual) -> String {
        return "".to_owned();
    }
}

fn parse_float(src: &str) -> Option<(f64, &str)> {
    if let Some(end) = src.find(')') {
        use std::str::FromStr;
        let (f_src, rest) = src.split_at(end);
        if let Ok(f) = f64::from_str(f_src) {
            return Some((f, rest));
        }
    }
    return None;
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub struct MaxFloat64(pub f64);

impl Eq for MaxFloat64 {}
impl Ord for MaxFloat64 {
    fn cmp(&self, other: &Self) -> Ordering {
        PartialOrd::partial_cmp(self, other).unwrap_or(Ordering::Equal)
    }
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub struct MaxFloat64Dual(pub f64);

impl Eq for MaxFloat64Dual {}
impl Ord for MaxFloat64Dual {
    fn cmp(&self, other: &Self) -> Ordering {
        PartialOrd::partial_cmp(self, other).unwrap_or(Ordering::Equal)
    }
}

impl TruthValue for MaxFloat64 {
    type Dual = MaxFloat64Dual;

    fn default() -> Self {
        MaxFloat64(1.0) 
    }

    fn zero() -> Self {
        MaxFloat64(0.0)
    }

    fn dual_default() -> Self::Dual {
        MaxFloat64Dual(1.0) 
    }

    fn either(a: &Self, b: &Self) -> Self {
        MaxFloat64(f64::max(a.0, b.0))
    }

    fn back_either(a: &Self, b: &Self, err: &Self) -> (Self, Self) {
        if a.0 >= b.0 {
            (*err, MaxFloat64(0.0))
        } else {
            (MaxFloat64(0.0), *err)
        }
    }

    fn both(a: &Self, b: &Self) -> Self {
        MaxFloat64(f64::min(a.0, b.0))
    }

    fn back_both(a: &Self, b: &Self, err: &Self) -> (Self, Self) {
        if a.0 <= b.0 {
            (*err, MaxFloat64(0.0))
        } else {
            (MaxFloat64(0.0), *err)
        }
    }

    fn finalize(dual: &Self::Dual, a: &Self) -> Self {
        MaxFloat64(dual.0 * a.0)
    }

    fn back_finalize(dual: &Self::Dual, _a: &Self, err: &Self) -> Self {
        MaxFloat64(err.0 / dual.0)
    }

    fn sub(a: &Self, b: &Self) -> Self {
        MaxFloat64(a.0 - b.0)
    }

    fn sum(a: &Self, b: &Self) -> Self {
        MaxFloat64(f64::max(-1.0, f64::min(1.0, a.0 + b.0)))
    }

    fn dual_adjust(a: &Self::Dual, b: &Self) -> Self::Dual {
        MaxFloat64Dual(f64::max(0.0, f64::min(1.0, a.0 + b.0)))
    }


    fn parse(src: &str) -> Option<(Self, &str)> {
        parse_float(src).map(|(f, rest)| (MaxFloat64(f), rest))
    }

    fn parse_dual(src: &str) -> Option<(Self::Dual, &str)> {
        parse_float(src).map(|(f, rest)| (MaxFloat64Dual(f), rest))
    }

    fn as_datalog(&self) -> String {
        return format!("confidence({}) ", self.0);
    }

    fn dual_as_datalog(dual: &Self::Dual) -> String {
        return format!("weight({}) ", dual.0);
    }
}
