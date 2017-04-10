
use rand;
use std::collections::hash_map::{HashMap, Entry};
use types::{Predicate, Constant, Variable, Literal, Clause, TermIndex, Term};
use fact_table::{FactTable};
use truth_value::{TruthValue};
use program::{Program};
use std::cmp;

pub struct Generator<R> where R: rand::Rng {
    rng: R,
    max_predicate: Predicate,
    num_terms: HashMap<Predicate, usize>,
    max_constant: HashMap<(Predicate, TermIndex), Constant>,
}

impl<R> Generator<R> where R: rand::Rng {
    pub fn new<T>(rng: R, facts: &FactTable<T>, program: &Program<T>) -> Self where T: TruthValue {
        let max_predicate = program.predicate_names.len();
        let mut max_constant = HashMap::new();
        for (fact, _) in facts.all_facts() {
            for (index, constant) in fact.terms.into_iter().enumerate() {
                match max_constant.entry((fact.predicate, index)) {
                    Entry::Occupied(mut pair) => {
                        let old_val = *pair.get();
                        pair.insert(cmp::max(old_val, constant));
                    },
                    Entry::Vacant(pair) => {
                        pair.insert(constant);
                    }
                }
            }
        }
        Generator {
            rng: rng,
            max_predicate: max_predicate,
            num_terms: program.predicate_num_terms.clone(),
            max_constant: max_constant,
        }
    }

    fn gen_predicate(&mut self) -> Predicate {
        // 0 through max, inclusive.
        self.rng.gen_range(0, 1 + self.max_predicate)
    }

    fn gen_constant(&mut self, predicate: Predicate, term_index: usize) -> Constant {
        // Might want to increase by one.
        let max_constant = 1 + *self.max_constant.get(&(predicate, term_index)).unwrap();
        // Want inclusive range.
        self.rng.gen_range(0, 1 + max_constant)
    }

    fn get_num_terms(&self, predicate: Predicate) -> usize {
        *self.num_terms.get(&predicate).unwrap()
    }

    fn gen_head(&mut self, max_body_len: usize) -> (Literal, usize) {
        let predicate = self.gen_predicate();
        let num_terms = self.get_num_terms(predicate);
        let num_output_variables = if self.rng.gen_weighted_bool(8) {
            // With 1/8 probability, select a random number of output variables.
            self.rng.gen_range(0, num_terms + 1usize)
        } else {
            // Otherwise, use all output terms as output_variables.
            num_terms
        };
        let mut num_remaining_output_variables = num_output_variables;
        let mut head_terms = Vec::with_capacity(num_terms);
        for (term_index, num_remaining_output_terms) in (1..(1 + num_terms)).rev().enumerate() {
            // If every remaining output term must be filled.
            let prob_should_use_output_variable = 
                num_remaining_output_variables as f64 / num_remaining_output_terms as f64;
            // TODO(zentner): This never uses the same output variable multiple times, which is
            // valid.
            let next_output_variable = num_output_variables - num_remaining_output_variables;
            if self.rng.next_f64() <= prob_should_use_output_variable  {
                head_terms.push(Term::Variable(next_output_variable));
                num_remaining_output_variables -= 1;
            } else {
                head_terms.push(Term::Constant(self.gen_constant(predicate, term_index)));
            }
        }
        let head = Literal::new_from_vec(predicate, head_terms);
        assert!(num_remaining_output_variables == 0);
        return (head, num_output_variables);
    }

    pub fn gen_clause(&mut self, max_body_len: usize) -> Clause {
        let (head, num_output_variables) = self.gen_head(max_body_len);
        let mut body_len = self.rng.gen_range(1, 1 + max_body_len);
        println!("initial body_len = {:?}", body_len);
        let mut body = Vec::with_capacity(body_len);
        let mut unused_output_variables: Vec<_> = (0..num_output_variables).map(|v| Some(v)).collect();
        self.rng.shuffle(&mut unused_output_variables);
        let mut num_unused_output_variables = num_output_variables;
        let mut num_variables = num_output_variables;
        let mut num_total_body_terms = 0;
        let mut predicates = Vec::with_capacity(body_len);
        let mut num_terms = Vec::with_capacity(body_len);
        while num_total_body_terms < num_output_variables || predicates.len() < body_len {
            let predicate = self.gen_predicate();
            predicates.push(predicate);
            let term_count = self.get_num_terms(predicate);
            num_terms.push(term_count);
            num_total_body_terms += term_count;
        }
        body_len = predicates.len();
        println!("predicates = {:?}", predicates);
        println!("num_terms = {:?}", num_terms);
        let mut num_remaining_output_terms: usize = num_total_body_terms;
        for (&predicate, &num_terms) in predicates.iter().zip(num_terms.iter()) {
            let mut terms = Vec::with_capacity(num_terms);
            for term_index in 0..num_terms {
                // If every remaining output term must be filled.
                let prob_should_use_output_variable = 
                    num_unused_output_variables as f64 / num_remaining_output_terms as f64;
                if self.rng.next_f64() <= prob_should_use_output_variable  {
                    let output_variable;
                    loop {
                        if let Some(variable) = unused_output_variables.pop().unwrap() {
                            output_variable = variable;
                            break;
                        }
                    }
                    terms.push(Term::Variable(output_variable));
                    num_unused_output_variables -= 1;
                } else {
                    if self.rng.gen_weighted_bool(3) {
                        // With 1/3 probability, output a constant.
                        terms.push(Term::Constant(self.gen_constant(predicate, term_index)));
                    } else {
                        // Might want to increase the number of variables by one.
                        let variable = self.rng.gen_range(0, 1 + num_variables);
                        num_variables = cmp::max(num_variables, variable);
                        if variable < num_output_variables {
                            // Mark the corresponding output variable as used.
                            if let Some(index) = unused_output_variables
                                    .iter()
                                    .position(|&v| v == Some(variable)) {
                                unused_output_variables[index] = None;
                                num_unused_output_variables -= 1;
                            }
                        }
                        terms.push(Term::Variable(variable));
                    }
                }
                num_remaining_output_terms -= 1;
            }
            let literal = Literal::new_from_vec(predicate, terms);
            body.push(literal);
        }
        let clause = Clause::new_from_vec(head, body);
        return clause;
    }
}

#[cfg(test)]
mod tests {
    use super::{Generator};
    use program::Program;
    use fact_table::FactTable;
    use parser::{program};
    use rand::XorShiftRng;
    use rand::SeedableRng;

    #[test]
    fn can_generate_a_clause() {
        let rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (facts, program, _) = program::<()>(r#"
        a(0).
        a(1).
        a(X) :- b(X)
        "#).unwrap().0;
        let mut generator = Generator::new(rng, &facts, &program);
        println!("generated_clause = {:?}", generator.gen_clause(4));
    }
}
