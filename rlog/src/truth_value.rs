

pub trait TruthValue : Clone + PartialEq {
    fn default() -> Self;
    fn join(&self, new: Self);
}

impl TruthValue for () {
    fn default() -> Self {}
    fn join(&self, _: Self) {}
}

