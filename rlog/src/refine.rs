use bottom_up::evaluate_bottom_up;
use fact_table::FactTable;
use generate::Generator;
use name_table::NameTable;
use optimize::{Adjustment, compute_adjustments};
use program::Program;
use rand::{Rng, XorShiftRng};
use reconstrain::compute_constraint_measure;
use std::io;
use std::io::Write;
use std::mem::swap;
use truth_value::TruthValue;

pub struct Refiner<R, T>
    where R: Rng,
          T: TruthValue
{
    rng: R,
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
    num_random_clauses_to_add: usize,
    num_mutated_clauses_to_add: usize,
    default_clause_weight: T::Dual,
}

impl<R, T> Refiner<R, T>
    where R: Rng,
          T: TruthValue
{
    pub fn new(mut rng: R,
               facts: FactTable<T>,
               program: Program<T>,
               samples: Vec<(FactTable<T>, FactTable<T>)>)
               -> Self {
        let mut default_clause_weight = T::dual_zero();
        T::dual_adjust(&mut default_clause_weight, &T::dual_default(), 0.5);
        let gen_rng = rng.gen::<XorShiftRng>();
        Refiner {
            rng: rng,
            generator: Generator::new(gen_rng, &facts, &program),
            base_facts: facts,
            program: program,
            samples: samples,
            gradient_iterations: 10,
            learning_rate: 0.5,
            max_new_body_len: 3,
            max_new_predicate_terms: 3,
            step_iterations: 100,
            clause_weight_cutoff_coeff: 1.0,
            num_random_clauses_to_add: 5,
            num_mutated_clauses_to_add: 5,
            default_clause_weight: default_clause_weight,
        }
    }

    pub fn fit_weights(&mut self) -> Adjustment<T> {
        let mut final_res = None;
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
            final_res = Some(res);
        }
        final_res.unwrap()
    }

    pub fn reconstrain(&mut self, adjust: Adjustment<T>) {
        let mut result_facts = self.base_facts.clone();
        evaluate_bottom_up(&mut result_facts, &self.program);
        let mut constraint = compute_constraint_measure(&self.program, &result_facts, &adjust);
        for _ in 0..self.num_mutated_clauses_to_add {
            constraint.insert_new_clause(&mut self.rng, &mut self.program);
        }
    }

    pub fn add_clauses(&mut self) {
        let mut result_facts = self.base_facts.clone();
        evaluate_bottom_up(&mut result_facts, &self.program);
        self.generator.update_max_constant(&result_facts);
        self.generator.update_num_terms(&self.program);
        for _ in 0..self.num_random_clauses_to_add {
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
        let mean_weight = self.program.mean_weight();
        let mut program = Program::new();
        swap(&mut program, &mut self.program);
        self.program.predicate_names = program.predicate_names.clone();
        for (clause_idx, (clause, weight)) in
            program
                .clause_iter()
                .cloned()
                .zip(program.clause_weights.iter().cloned())
                .enumerate() {
            if T::dual_less(&mean_weight, &weight, self.clause_weight_cutoff_coeff) {
                let clause_var_names = program
                    .clause_variable_names
                    .get(&clause_idx)
                    .cloned()
                    .unwrap_or_else(|| NameTable::new());
                self.program
                    .push_clause(clause, weight, clause_var_names)
                    .unwrap();
            }
        }
        self.check_num_fact_terms();
    }

    fn check_num_fact_terms(&mut self) {
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
            print!(".");
            io::stdout().flush().unwrap();
            self.add_clauses();
            let adjustments = self.fit_weights();
            self.reconstrain(adjustments);
            self.reduce_clauses();
        }
        println!();
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

    //#[test]
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
        refiner.iterate(5);
        println!("program = {}", refiner.get_program());
    }
}
