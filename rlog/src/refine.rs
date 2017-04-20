use bottom_up::evaluate_bottom_up;
use fact_table::FactTable;
use generate::Generator;
use name_table::NameTable;
use optimize::compute_adjustments;
use program::Program;
use rand::{Rng, XorShiftRng};
use std::mem::swap;
use truth_value::TruthValue;

pub struct Refiner<T>
    where T: TruthValue
{
    generator: Generator<XorShiftRng>,
    base_facts: FactTable<T>,
    program: Program<T>,
    samples: Vec<(FactTable<T>, FactTable<T>)>,
    gradient_iterations: usize,
    learning_rate: f64,
    max_new_body_len: usize,
    max_new_predicate_terms: usize,
    step_iterations: usize,
    clause_weight_cutoff_coeff: f64,
    num_clauses_to_add: usize,
    default_clause_weight: T::Dual,
}

impl<T> Refiner<T>
    where T: TruthValue
{
    pub fn new<R>(mut rng: R,
                  facts: FactTable<T>,
                  program: Program<T>,
                  samples: Vec<(FactTable<T>, FactTable<T>)>)
                  -> Self
        where R: Rng
    {
        let mut default_clause_weight = T::dual_zero();
        T::dual_adjust(&mut default_clause_weight, &T::dual_default(), 0.5);
        Refiner {
            generator: Generator::new(rng.gen::<XorShiftRng>(), &facts, &program),
            base_facts: facts,
            program: program,
            samples: samples,
            gradient_iterations: 10,
            learning_rate: 0.5,
            max_new_body_len: 8,
            max_new_predicate_terms: 8,
            step_iterations: 100,
            clause_weight_cutoff_coeff: 1.0,
            num_clauses_to_add: 5,
            default_clause_weight: default_clause_weight,
        }
    }

    pub fn fit_weights(&mut self) {
        for _ in 0..self.gradient_iterations {
            let res = compute_adjustments(&self.program,
                                          &self.base_facts,
                                          &self.samples,
                                          self.step_iterations);
            for (clause_idx, adjustment) in res.clause_adjustments.iter().enumerate() {
                T::dual_adjust(&mut self.program.clause_weights[clause_idx],
                               adjustment,
                               self.learning_rate);
            }
        }
    }

    pub fn add_clauses(&mut self) {
        let mut result_facts = self.base_facts.clone();
        evaluate_bottom_up(&mut result_facts, &self.program);
        self.generator.update_max_constant(&result_facts);
        self.generator.update_num_terms(&self.program);
        for _ in 0..self.num_clauses_to_add {
            self.program
                .push_clause(self.generator
                                 .gen_clause(self.max_new_body_len,
                                             self.max_new_predicate_terms),
                             self.default_clause_weight.clone(),
                             NameTable::new())
                .unwrap();
        }
    }

    pub fn reduce_clauses(&mut self) {
        let num_weights = self.program.clause_weights.len();
        let amount_per_weight = 1f64 / num_weights as f64;
        let mut mean_weight = T::dual_zero();
        for weight in self.program.clause_weights.iter() {
            T::dual_adjust(&mut mean_weight, weight, amount_per_weight);
        }
        let mut program = Program::new();
        swap(&mut program, &mut self.program);
        self.program.predicate_names = program.predicate_names.clone();
        let clauses = program.clauses;
        let weights = program.clause_weights;
        for (clause_idx, (clause, weight)) in clauses.into_iter().zip(weights).enumerate() {
            if T::dual_less(&mean_weight, &weight, self.clause_weight_cutoff_coeff) {
                let clause_var_names = program
                    .clause_variable_names
                    .remove(&clause_idx)
                    .unwrap_or_else(|| NameTable::new());
                self.program
                    .push_clause(clause, weight, clause_var_names)
                    .unwrap();
            }
        }
        for &(ref input, ref output) in &self.samples {
            self.program.check_num_fact_terms(input).unwrap();
            self.program.check_num_fact_terms(output).unwrap();
        }
        self.program
            .check_num_fact_terms(&self.base_facts)
            .unwrap();
    }

    pub fn iterate(&mut self, iterations: usize) {
        for _ in 0..iterations {
            self.add_clauses();
            self.fit_weights();
            self.reduce_clauses();
        }
    }

    #[cfg(test)]
    pub fn get_program(&self) -> &Program<T> {
        &self.program
    }

    pub fn to_program(self) -> Program<T> {
        self.program
    }
}


#[cfg(test)]
mod tests {
    use super::Refiner;
    use parser::program;
    use rand::SeedableRng;
    use rand::XorShiftRng;
    use truth_value::MaxFloat64;

    #[test]
    fn can_refine_single_clause() {
        let rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (facts, program, samples) = program::<MaxFloat64>(r#"
        types(0) :- a(0), b(0)
        sample
            b(1)
        output
            a(1).
        "#)
                .unwrap()
                .0;
        let mut refiner = Refiner::new(rng, facts, program, samples);
        refiner.iterate(10);
        println!("program = {}", refiner.get_program());
    }

    #[test]
    fn can_refine_conjunction() {
        let rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (facts, program, samples) = program::<MaxFloat64>(r#"
        types(0) :- a(0), b(0), c(0)
        sample
            b(1),
            c(1)
        output
            a(1).
        sample
            b(2),
            c(2)
        output
            a(2).
        sample
            b(3),
            c(3)
        output
            a(3).
        sample
            b(2),
            c(3)
        output
            types(0).
        "#)
                .unwrap()
                .0;
        let mut refiner = Refiner::new(rng, facts, program, samples);
        refiner.iterate(1);
        println!("program = {}", refiner.get_program());
    }
}
