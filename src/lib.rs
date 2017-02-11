mod types;
mod matching;

use types::{Predicate, Term, Literal, Clause};

//
// Bottom up evaluation strategy.
// Start with a (FIFO) queue of new facts.
// Go through each rule, and see if they prove a new fact given the known clauses.

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
