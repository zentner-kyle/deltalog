pub trait TruthValue : Clone + PartialEq {
    type Dual;
    fn default() -> Self;
    fn either(&self, other: &Self) -> Self;
    fn both(&self, other: &Self) -> Self;
}

impl TruthValue for () {
    type Dual = ();
    fn default() -> Self {}
    fn either(&self, _: &Self) -> Self {}
    fn both(&self, _: &Self) -> Self {}
}
