use fact_table::FactTable;
use program::Program;
use rand;
use std::cmp;
use std::collections::hash_map::{Entry, HashMap};
use std::fmt;
use truth_value::TruthValue;
use types::{Clause, Constant, Literal, Predicate, Term, TermIndex};

pub struct Generator<R>
    where R: rand::Rng
{
    rng: R,
    max_predicate: Predicate,
    num_terms: HashMap<Predicate, usize>,
    max_constant: HashMap<(Predicate, TermIndex), Constant>,
}

impl<R> Generator<R>
    where R: rand::Rng
{
    pub fn new<T>(rng: R, facts: &FactTable<T>, program: &Program<T>) -> Self
        where T: TruthValue
    {
        let max_predicate = program.predicate_names.len();
        let max_constant = HashMap::new();
        let mut result = Generator {
            rng: rng,
            max_predicate: max_predicate,
            num_terms: program.predicate_num_terms.clone(),
            max_constant: max_constant,
        };
        result.update_max_constant(facts);
        return result;
    }

    pub fn update_num_terms<T>(&mut self, program: &Program<T>)
        where T: TruthValue
    {
        self.num_terms = program.predicate_num_terms.clone();
    }

    pub fn update_max_constant<T>(&mut self, facts: &FactTable<T>)
        where T: TruthValue
    {
        self.max_constant = facts.max_constant_table();
    }

    fn gen_predicate(&mut self) -> Predicate {
        // 0 through max, inclusive.
        self.rng.gen_range(0, 1 + self.max_predicate)
    }

    fn gen_constant(&mut self, predicate: Predicate, term_index: usize) -> Constant {
        // Might want to increase by one.
        let max_constant = 1 +
                           self.max_constant
                               .get(&(predicate, term_index))
                               .cloned()
                               .unwrap_or(0usize);
        // Want inclusive range.
        self.rng.gen_range(0, 1 + max_constant)
    }

    fn get_num_terms(&mut self, predicate: Predicate, max_new: usize) -> usize {

        match self.num_terms.entry(predicate) {
            Entry::Occupied(pair) => *pair.get(),
            Entry::Vacant(pair) => {
                let n = self.rng.gen_range(0, 1 + max_new);
                pair.insert(n);
                n
            }
        }
    }

    fn gen_head(&mut self, max_new_predicate_terms: usize) -> (Literal, usize) {
        let predicate = self.gen_predicate();
        let num_terms = self.get_num_terms(predicate, max_new_predicate_terms);
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
            let prob_should_use_output_variable = num_remaining_output_variables as f64 /
                                                  num_remaining_output_terms as f64;
            // TODO(zentner): This never uses the same output variable multiple times, which is
            // valid.
            let next_output_variable = num_output_variables - num_remaining_output_variables;
            if self.rng.next_f64() <= prob_should_use_output_variable {
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

    pub fn gen_clause(&mut self, max_body_len: usize, max_new_predicate_terms: usize) -> Clause {
        let (head, num_output_variables) = self.gen_head(max_new_predicate_terms);
        let body_len = self.rng.gen_range(1, 1 + max_body_len);
        let mut body = Vec::with_capacity(body_len);
        let mut unused_output_variables: Vec<_> =
            (0..num_output_variables).map(|v| Some(v)).collect();
        self.rng.shuffle(&mut unused_output_variables);
        let mut num_unused_output_variables = num_output_variables;
        let mut num_variables = num_output_variables;
        let mut num_total_body_terms = 0;
        let mut predicates = Vec::with_capacity(body_len);
        let mut num_terms = Vec::with_capacity(body_len);
        while num_total_body_terms < num_output_variables || predicates.len() < body_len {
            let predicate = self.gen_predicate();
            predicates.push(predicate);
            let term_count = self.get_num_terms(predicate, max_new_predicate_terms);
            num_terms.push(term_count);
            num_total_body_terms += term_count;
        }
        let mut num_remaining_output_terms: usize = num_total_body_terms;
        for (&predicate, &num_terms) in predicates.iter().zip(num_terms.iter()) {
            let mut terms = Vec::with_capacity(num_terms);
            for term_index in 0..num_terms {
                // If every remaining output term must be filled.
                let prob_should_use_output_variable = num_unused_output_variables as f64 /
                                                      num_remaining_output_terms as f64;
                if self.rng.next_f64() <= prob_should_use_output_variable {
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
                        // Might want to increase the number of variables by two.
                        let variable = self.rng.gen_range(0, 2 + num_variables);
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

impl<R> fmt::Debug for Generator<R>
    where R: rand::Rng
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f,
               "Generator {{ rng: *, max_predicate: {:?}, num_terms: {:?}, max_constant: {:?} }}",
               self.max_predicate,
               self.num_terms,
               self.max_constant)
    }
}

#[cfg(test)]
mod tests {
    use super::Generator;
    use parser::program;
    use rand::SeedableRng;
    use rand::XorShiftRng;

    #[test]
    fn can_generate_a_clause() {
        let rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (facts, program, _) = program::<()>(r#"
        a(0).
        a(1).
        a(X) :- b(X)
        "#)
                .unwrap()
                .0;
        let mut generator = Generator::new(rng, &facts, &program);
        println!("generated_clause = {:?}", generator.gen_clause(4, 8));
        println!("generated_clause = {:?}", generator.gen_clause(100, 8));
    }
}
