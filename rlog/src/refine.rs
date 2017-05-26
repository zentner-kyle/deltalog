use bottom_up::evaluate_bottom_up;
use fact_table::FactTable;
use generate::Generator;
use mutate::{MutationOpGenerator, MutationState};
use optimize::{Adjustment, compute_adjustments};
use program::Program;
use rand::{Rng, XorShiftRng};
use reconstrain::compute_constraint_measure;
use selector::Selector;
use std::cmp::Ordering;
use std::cmp::PartialOrd;
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
    num_random_clauses_to_add: usize,
    num_mutated_clauses_to_add: usize,
    default_clause_weight: T::Dual,
    max_num_clauses: usize,
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
            num_random_clauses_to_add: 5,
            num_mutated_clauses_to_add: 5,
            default_clause_weight: default_clause_weight,
            max_num_clauses: 10,
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
                T::dual_adjust(self.program.clause_weight_mut(clause_idx),
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
        let gen = MutationOpGenerator::uniform();
        for _ in 0..self.num_mutated_clauses_to_add {
            if let Ok(clause_idx) = constraint.choose_clause(&mut self.rng, &self.program) {
                let mut clause = self.program.get_clause_by_idx(clause_idx).clone();
                let mut state = MutationState::new(&clause);
                let mut success = false;
                for _ in 0..10 {
                    if let Ok(body_op) = gen.generate_body(&mut self.rng,
                                                           &mut constraint,
                                                           &self.program,
                                                           0) {
                        success |= state.checked_apply_body_mut_op(body_op,
                                                                   &mut clause,
                                                                   &mut self.rng,
                                                                   &self.program);

                    }
                    if let Ok(head_op) = gen.generate_head(&mut self.rng,
                                                           &mut constraint,
                                                           &self.program,
                                                           0) {
                        success |= state.checked_apply_head_mut_op(head_op,
                                                                   &mut clause,
                                                                   &mut self.rng,
                                                                   &self.program);
                    }
                }
                if success {
                    clause.canonicalize_in_place();
                    self.program.push_clause_simple(clause);
                }
            }
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
                             None)
                .unwrap();
        }
    }

    pub fn reduce_clauses(&mut self) {
        let mut clauses_to_keep = (0..self.program.num_clauses()).collect::<Vec<_>>();
        clauses_to_keep.sort_by(|a, b| {
                                    let a_weight = T::dual_mag(self.program.clause_weight(*a));
                                    let b_weight = T::dual_mag(self.program.clause_weight(*b));
                                    b_weight
                                        .partial_cmp(&a_weight)
                                        .unwrap_or(Ordering::Equal)
                                });
        clauses_to_keep.truncate(self.max_num_clauses);
        let mut program = Program::new();
        swap(&mut program, &mut self.program);
        *self.program.predicate_names_mut() = program.predicate_names().clone();
        for clause_idx in clauses_to_keep {
            let clause = program.get_clause_by_idx(clause_idx);
            let mut weight = program.clause_weight(clause_idx).clone();
            let clause_var_names = program.clause_variable_names.get(&clause_idx).cloned();
            T::dual_adjust(&mut weight, &T::dual_default(), -0.1);
            self.program
                .push_clause(clause.clone(), weight, clause_var_names)
                .unwrap();
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
            println!("computing adjustment");
            let adjustments = self.fit_weights();
            println!("reconstraining");
            self.reconstrain(adjustments);
            println!("reducing clauses");
            self.reduce_clauses();
            println!("adding clauses");
            self.add_clauses();
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

    #[test]
    #[allow(dead_code)]
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
        refiner.iterate(2);
        println!("program = {}", refiner.get_program());
    }

    #[test]
    fn can_refine_conjunction() {
        // Try to refine a(X) :- b(X), c(X)
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
            b(1),
            c(2)
        output.
        "#)
                .unwrap()
                .0;
        let mut refiner = Refiner::new(rng, facts, program, samples);
        for _ in 0..2 {
            refiner.iterate(1);
            println!("program = {}", refiner.get_program());
        }
    }
}
