use std::fmt::{Debug};

pub trait TruthValue : Clone + PartialEq + Debug {
    type Dual;
    fn default() -> Self;
    fn either(&self, other: &Self) -> Self;
    fn both(&self, other: &Self) -> Self;
    fn parse(&str) -> Option<(Self, &str)>;
    fn as_datalog(&self) -> String;
}

impl TruthValue for () {
    type Dual = ();
    fn default() -> Self {}
    fn either(&self, _: &Self) -> Self {}
    fn both(&self, _: &Self) -> Self {}
    fn parse(_: &str) -> Option<(Self, &str)> { None }
    fn as_datalog(&self) -> String {
        return "".to_owned();
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct MaxFloat64(f64);

impl TruthValue for MaxFloat64 {
    type Dual = MaxFloat64;

    fn default() -> Self {
        MaxFloat64(1.0) 
    }

    fn either(&self, other: &Self) -> Self {
        MaxFloat64(f64::max(self.0, other.0))
    }

    fn both(&self, other: &Self) -> Self {
        MaxFloat64(f64::min(self.0, other.0))
    }

    fn parse(src: &str) -> Option<(Self, &str)> {
        if let Some(end) = src.find(')') {
            use std::str::FromStr;
            let (f_src, rest) = src.split_at(end);
            if let Ok(f) = f64::from_str(f_src) {
                return Some((MaxFloat64(f), rest));
            }
        }
        return None;
    }

    fn as_datalog(&self) -> String {
        return format!("weight({}) ", self.0);
    }
}
