The origins of this project are in discovering the fundamental weaknesses of
the rvlvr project. One of those weaknesses is that evolutionary criteria, even
behaviorally aware ones guide the search of the program space too strictly. For
example, consider how much of the space will inevitably be explored in order to
find a program like "not (n % 2 == 0 and n > 4 * k)". Because of the presence
of the not operator, the MSE of related programs (for example, the same program
without the not operator) is very high. Since rvlvr only builds programs by
concatenation, the search guide fails completely here and basically forces
rvlvr from ever evolving the highest fitness program. However, it should
eventually evolve a similar or equivalent program, by "refactoring" the
location of the not operator.

This leads to the question, what part of the program space is actually
effectively searched using search guides like this one? It would seem that
search guides like this one help genetic algorithms where they are already most
effective (and least necessary): smooth reward functions, with little distance
between local minima. This is also what gradient based methods are good at,
although gradient based methods may suffer from additional issues. In
particular, the gradient may be difficult to compute, or the gradient may not
exist, even though the reward function is smooth enough for evolutionary
algorithms.

An interesting space to consider is that of convex logic. This is the space of
logic programs which operate on a set of facts, where each fact is a predicate
over some fixed (per predicate) number of atoms. Without negation (or with only
very limited negation in the form of pre-processing) addition of new facts to
the set can never cause the removal of old facts. However, this space of
programs is still very versatile, because many logical conditions can be
reorganized into a form that doesn't require the use of a negation operator.

Unlike integer program evolution, this program space naturally allows for
handling an ensemble of programs, since clauses operate largely independently.
Furthermore, due to uses in database queries, the most common implementation of
programs in this space, datalog, already has significant literature on how to
implement efficient recomputation of the results of a program when a new clause
is added to it.

For efficiency, we should probably create a datalog variant where each clause
and each literal is a fixed size. However, it's not clear to me if a particular
size limits the abilities of programs.

After spending a few hours reading the research, it appears there's no
concensus on if limiting the order of predicates limits the capabilities of the
space.

One annoyance is that my original problem domain for this work is actually very
hard to fit into datalog. Nevermind, I'm just bad at datalog.


Connect 4:

Board(color: 0..3, x: 0..8, y: 0..7)
Turn(player: 0..2)
Next(option: 0.., color: 0..3, x: 0..8, y: 0..7)

Player(color: 0..3)
Player(0)
Player(1)

Next(x, color, x, y) :- Board(2, x, 0), Turn(color)
Next(x, color, x, y) :- Board(2, x, y), Board(piece, x, y1), Subtract(y, 1, y1), Turn(color), Player(piece)


Pawn Only Chess:

Board(color: 0..3, x:0..9, y: 0..9)
Turn(player: 0..2)
Next(option: 0.., color: 0..3, x: 0..9, y: 0..9)

Player(color: 0..3)
Player(0)
Player(1)

Direction(0, 1)
Direction(1, -1)

DiagDirection(1)
DiagDirection(-1)

Next(e, color, x, y), Next(e, 2, x, y1) :- Board(2, x, y), Turn(color), Board(color, x, y1), Add(y, direction, y1), Direction(color, direction), Enumerate3(x, y, 0, e)
Next(e, color, x, y), Next(e, 2, x1, y1) :- Board(other, x, y), Turn(color), Other(other, color), Board(color, x1, y1), Add(y, direction, y1), Direction(color, direction), Enumerate(x, y, direction1, e), Add(x, direction2, x1), DiagDirection(direction1)

What should our derivation algorithm be, and how can it be described using
linear algebra? The effectiveness of deep neural networks makes it clear that
using gradients to optimize large models can be very effective. I do not think
that there is a reasonable way to compute a gradient in the space of all
datalog programs. However, it would still be convenient if there is a way to
use search guides similar to back propagation to make the evolutionary search
more efficient.

On the topic of evoluutionary search, it's clear that the general approach here
has no need to be truly evolutionary in nature. A family of related datalog
programs will all tend to produce a common subset of facts. We could record,
for each fact, which individuals in the population derive that fact. This would
probably be more efficient than computing the result of each individual in the
population separately. However, this does involve more overhead than simply
recording how many members of the population derive this fact. Of course, if we
only record how many members of the population derive a fact, we cannot use
this information to derive new facts. But if we instead of a count use a
"weight", then we have similar semantics, but less overhead. Which is probably
preferable.

The evolutionary fitness criteria should probably be something like "minimize
the weights of invalid facts, while maximizing the weight of valid facts." This
is slightly complicated by the presence of enumeration. A fact is valid if it
is expected, ignoring the presence of its enumeration value. A fact is invalid
if it is not expected. A fact is very invalid if it has the same enumeration
value as a valid fact. There should probably also be some weak evolutionary
pressure towards simpler programs.

One weakness of rvlvr was that the fitness function was finicky and ad hoc.
Perhaps we can avoid this by constraining weights this time around?

Alright, what exactly is the error propagation algorithm?

We input our input facts X. Then, we compute an activation for each fact that
has non-zero activation. An activation is essentially a number of times that
fact has been derived true by the datalog program. Note that since a clause may
appear more than once in the program (have non-zero weight), this number is the
sum of (the weights of each clauses implying the fact, times the sum, for each
set of valid tail combinations, of the activation of each fact in the tail). We
record how much each clause contributes to the fact.

It occurs to me that the "back propagation" algorithm I'm about to specify
performs much of the same computation as top-down datalog evaluation. Perhaps
we would effectively compute both together?

That would be difficult to do if we don't know if we can't know whether a fact
is valid in isolation. This runs counter to how enumeration was supposed to be
enforced. However, maybe enumeration can still be enforced by propagating
inequality constraints along with equality constraints. Top down algorithms
need to use equality constraints anyways. However, the inequality constraints
needed here are soft, unlike the equality constraints, which are hard.

I don't see significant evidence that top-down approaches would be more
efficient, so I think I'll continue with the bottom up method.

It's worth thinking about how to implement refinement of datalog program
ensembles using traditional back-propagation algorithms. In principle, one
could imagine some way of encoding the refinement process as an unusual set of
error functions applied to some unusual structure of matrices.

Consider the space of all neural networks as the space of any finite sequence
of finite dimensional tensors with reLU's between them, and with arbitrary
bindings between the weights of different neurons. Then any activation tensor
represents a sequence of facts, where each activation represents what
unweighted portion of the ensemble found that fact to be true. Furthermore,
because we used arbitrary dimensional tensors, each index into the activation
tensor can correspond to a constant, excluding the first index, which is needed
to indicate which predicate the fact is over. Furthermore, if we allow negative
weights we are implicitly computing an ensemble of datalog programs with
stratified negation. It's stratisfied because a finite sequence of weight
tensors was assumed. However, variables make no appearance in this
representation.  Furthermore, the size of the tensors grows exponentially, with
the exponent being the arity of the largest predicate and the base being the
number of constants. It's also worth mentioning that a constant needs to be
input to each layer to allow conjunction to be represented.

How could variables be represented? Imagine we have a very simple dataflow
program:

IDB:

a(X: 0..2)
b(X: 0..2)
c(X: 0..2)

a(X) :- b(X), c(X)

EDB:
b(0)
c(0)

Then the tensor form is:

A[0] = [
 [0, n], # a
 [1, m], # b
 [1, k], # c
 ]

And we would like:

A[1] = reLU(A[0] * X) = [
 [1, n], # a
 [1, m], # b
 [1, k], # c
 ]

For all n, m, k.

We would also like

reLU([
 [0, n], # a
 [0, m], # b
 [1, k], # c
 ] * X) = [
 [0, n], # a
 [0, m], # b
 [1, k], # c
 ]

and

reLU([
 [0, n], # a
 [1, m], # b
 [0, k], # c
 ] * X) = [
 [0, n], # a
 [1, m], # b
 [0, k], # c
 ]

From these examples, the pattern of broadcasting across each dimension
represents a variable and multiply the rest of the tensors together is clear.
However, it's also clear from this analysis that this technique isn't going to
get us very far. Firstly, it seems that you need at least one layer per clause.
Secondly, there is certainly going to be significant issues with the gradient.
If reLU is actually used, then the gradient will usually be zero in the
direction of each reasonable change. Using the sigmoid function should help
somewhat. Finally, this method is dense, but it encodes the entire space.
That's a rather large optimization space.

Even if I do end up implementing gradient based optimization, I doubt other
code I write will be wasted, so I'm not going to focus on it just yet.








Red(x: 0..6, y: 0..5)
Black(x: 0..6, y: 0..5)
Empty(x: 0..6, y: 0..5)
Turn(player: 0..1)

NextRed(option: 0.., x: 0..6, y: 0..5)
NextBlack(option: 0.., x: 0..6, y: 0..5)

Piece(x, y) :- Red(x, y)
Piece(x, y) :- Black(x, y)

NextRed(x, x, 0) :- Empty(x, 0), Turn(0)
NextRed(x, x, y) :- Empty(x, y), Turn(0), Piece(x, y1), Subtract(y, 1, y1)

Next(x, player, x, y) :- Valid(x, y, color), Board(player, _, _), Pla
Empty(x, 0), Turn(player)


Red(turn: 0.., x: 0..6, y: 0..5)
Black(turn: 0.., x: 0..6, y: 0..5)
Empty(turn: 0.., x: 0..6, y: 0..5)

Next(option: 0.., turn: 0.., x: 0..6, y: 0..5)


Piece(turn, x, y) :- Red(turn, x, y)
Piece(turn, x, y) :- Black(turn, x, y)

NextPiece(x, 0) :- Piece(x, 0)


One conceptual problem we've been hitting is that it's not clear how to handle
really computing activations in a useful, stable way. The first, obvious
problem is recursive self-implication. When a fact leads to a set of new facts
which imply that fact, as long as the weights along the loop multiply to a
number above one then the activation becomes infinite. It's only minorly
difficult to use tabling to detect this case, and either record the activation
for all such facts as infinite, but it's not clear that's a useful result.

Of course, this can be mitigated somewhat by forcing activations to fall off,
either by enforcing that weights are all less than one or by multiplying by
some fall off factor. However, this could also cause vanishing gradient type
problems, where the activations all become near zero.

On a related note, it's not clear that demand driven mutation really matches
gradient methods all that closely. The partial differential of the output table
with respect to the introduction of any particular new rule is often zero, even
though a small set of rules can result in total reduction of error.

We've been thinking about this pretty backwards. If you think of this as a
gradient problem, then there's a lot of problems. It's possible to actually
compute the gradient fully in a sparse way using typical differential methods.
However, actually changing the structure of the program doesn't fit gradient
methods at all. Furthermore, thinking about it that way doesn't really help
make progress. Instead, we want to think about exactly what operations we're
trying to do last, and then reason about what data dependencies are useful for
doing that.

What we really want to capture from the logic program is a direction to mutate
the program in. In other words, we want to extract some data that tells us
something about what rules to introduce to the program, or, if those rules
already exist, strengthen the weights of.

Think about how to evaluate the datalog program in a top-down fashion. We start
with some predicate we would like to find matches for. First, we look through
the fact table for this predicate. Then, we look through clauses with that
predicate in their head. For each such clause, we need to create a set of
constraints for that clause. We enter our search into the table for that
clause. Then, we recurse into each literal in the body of the clause, carrying
our constraints with us. This recursive process fills out the set of all
possible facts for our predicate into its fact table, which we return.
Subsumtive tabling operates in the same way, although it orders the search, and
keeps fact tables per literal, instead of per predicate.

So, to think about how to mutate the program, think about each step in this
process where the input could be changed to get the output we want.

Example:
Say we would like to derive the following datalog program:

```
  foo(X, Y) :- bar(X, X), bar(Y, Y)
```

Given the following samples:
```
given
  bar(0, 0).
  bar(1, 1).
implies
  foo(0, 1).
  foo(1, 0).
given
  bar(2, 2).
  bar(1, 1).
implies
  foo(2, 1).
  foo(1, 2).
given
  bar(0, 0).
  bar(2, 2).
implies
  foo(0, 2).
  foo(2, 0).
```

Given the following training routine (the empty program):
```
fix
  foo(X, Y)
```

We start by looking through the table of values for foo. It's empty, so maybe
we should add a new fact. We should somehow record this demand. Then, we look
through the rules implying foo. There are none, so maybe we should add one.
Record this demand. There's nothing left for us to do, so we're done computing
demand.

What is our demand? Either a fact which fulfills some constraints, or a rule
which would produce this fact from the same constraints.

What exactly are our constraints? Usually, in top down evaluation we're trying
to find all facts which can fit our predicate. Fortunately, in our case we
should have much more specific constraints. We're looking for specific facts,
not all facts for a predicate. Our constraints, at any point in time, are
actually a finite set of facts we need to prove, given our set of inputs.


It seems likely that we'll need to lose some information when coming up with
the mutation guides. My current best idea is to record for each variable a
vector of values it holds throughout a run for a given input database. Then, we
can compute a similar vector based on how much each variable violates
constraints. Hopefully, we can use these two vectors to guide the mutation.

What types of mutation should we perform? Within a clause, if a variable is
over constrained then we can replace some uses of it by new, unconstrained
variables. Alternatively, we might want to delete a literal referring to the
variable.

If a variable needs to be more constrained, we can merge it with another
variable, or add another literal referring to that variable.


Alright, back to implementation mode. Let's start by implementing minimal
backwards reasoning.
