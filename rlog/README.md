DeltaLog
========

DeltaLog is a library for evaluating, optimizing, and synthesizing a datalog
dialect. It's implemented entirely in safe Rust.

Current features include:
 * Parsing.
 * In-memory bottom-up evaluation.
 * Evaluation using a form a fuzzy logic.
 * Reverse mode automatic differentiation.
 * Weight optimization of a progam using gradient descent.
 * Generation of random programs.
 * Mutation of programs.

Planned / work in progress features include:
 * Genetic optimization.
 * Synthesis.
 * Reverse inference.

##Parsing
The parser is a simple recursive descent parser. Rust's lightweight syntax for
Result based error handling makes this very easy.

##Evaluation
The evaluator uses the bottom up semi-naive algorithm.

##Fuzzy Logic and Clause Weights
DeltaLog allows specifying a TruthValue type. TruthValue types define how
union and intersection of sets of facts result in new TruthValues. Currently,
there are two TruthValue types, the empty tuple type, and a TruthValue type
that uses the maximum function for union, and the minimum function for
intersection.

##Reverse Mode Automatic Differentiation
DeltaLog can compute the adjoints of the weights associated with clauses, given
some loss values associated with some facts.

Currently, this requires that the TruthValue being used have various poorly
specified properties. In the future, this will be made more flexible.

Currently, the loss values are initialized to the mean squared error of the
expected set of facts for some set of predicates relative to the facts computed
given some input facts for the same set of predicates.

In the future, this will be made more flexible.

##Weight Optimization
DeltaLog can use the adjoints computed using Reverse Mode Automatic
Differentiation to adjust the weights of a program.

##Random Program Generation
DeltaLog is capable of generating random programs. However, the current method
for doing so is extremely ad-hoc. In the future, generation will be based on
DeltaLog's mutation capabilities, which are more principled.

##Program Mutation
DeltaLog's mutation algorithm is capable of being guided to prefer specific
predicates or terms within the literals of a predicate. It can be
configured to mutate within some sub-space of the space of DeltaLog programs.
For example, it can ensure that the mutated program has a maximum number of
literals per clause. It can also be tuned to prefer different types of
mutations. For example, it can be made to prefer insertion of new variables
over new constants. Finally, the mutation algorithm ensures that the resulting
program is well-formed.
