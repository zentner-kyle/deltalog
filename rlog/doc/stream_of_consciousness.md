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
  bar(0, 0),
  bar(1, 1)
implies
  foo(0, 1),
  foo(1, 0)
given
  bar(2, 2),
  bar(1, 1)
implies
  foo(2, 1),
  foo(1, 2)
given
  bar(0, 0),
  bar(2, 2)
implies
  foo(0, 2),
  foo(2, 0)
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
backwards reasoning. That is to say, given an asserted fact and a program, find
a set of sets of facts, such that any set of facts being true would result in
the asserted fact being true.


After implementing fuzzy logic, I'm fairly certain that  it is quite reasonable
to optimize the weights separately from the topology of the program. Weight
adjustments are basically a table mapping clauses to weight reductions which
would minimize the output error. However, the mutation guides are basically a
table which map each variable in some clauses to some record of how that
variable is contributing to error. One specific idea would be a scalar, where
larger vlues indicate the variable should be less constrained, and negative
values indicate the variable should be more constrained.

Algorithm:
loop
  compute results
  compute weight adjustments
  if weight adjustments < threshold:
    break
  remove low weight clauses
  compute mutation guides
  generate new clauses


There's basically no way to ensure that this differential technique is correct
in certain ways without constraining the either, both, and finalize functions to
all be monotonic with respect to each other.

Do the current min-max weights have this problem? It's kinda hard to tell. I
believe they do if negative weights are allowed. The problem can be seen in the
following example:

confidence(1)
B(0)
C(0)

weight(-1)
A(X) :- B(X)
B(X) :- C(X)

What is the truth value of A(0) in this program?
If We evaluate A(X) :- B(X) first, then we both the truth value of B(0) with
the default to get the truth value 1 (= min(1, 1)). Then, we finalize 1 by
multiplying by the weight, -1 to get -1. So we assign A(0) the truth value -1.
Next, we evaluate B(X) :- C(X). Similarly, we take -1 * min(1, 1) = -1. Then,
we set the truth value of B(0) to either(-1, -1) = max(-1, -1) = 1. We evaluate
A(X) :- B(X) again, resulting in A(0) = -1 * max(-1, 


Consider the equivalent program, with positive weights (but some negative
confidence values):

confidence(1)
B(0)

weight(1)
A(X) :- B(X)
B(X) :- C(X)

confidence(-1)
C(0)

What is the truth value of A(0) in this program?
If We evaluate A(X) :- B(X) first, then we both the truth value of B(0) with
the default to get the truth value 1 (= min(1, 1)). Then, we finalize 1 by
multiplying by the weight, 1 to get 1. So we assign A(0) the truth value 1.
Next, we evaluate B(X) :- C(X). Similarly, we take 1 * min(1, -1) = -1. Then,
we set the truth value of B(0) to either(-1, 1) = max(-1, 1) = 1.



Perhaps we can make more progress on this problem by only considering trees? And passing values along the edges of trees?
We're looking for cases where two different order of evaluations of clauses results in a fact having two different truth values.
In the truth value of a fact is given by a reduction of either() of the
finalize() of a reduction of both(). In other words the minimal expression
which allows any particular operator to be reordered is (where a, b, c, and d are known truth values):
either(default(), finalize(both(default(), a, b)), finalize(both(default(), c, d)))

With the min-max truth value, that expression is equivalent to:
max(1.0, x * min(1.0, a, b), y * min(1.0, c, d))
where x and y are known weights

Assuming x and y are positive:
max(1.0, x * min(1.0, a, b), y * min(1.0, c, d)) = 
max(1.0, min(x, xa, xb), min(y, yc, yd))

a * min(max(b, c), min(max(d, e), max(f, g))) != a * min(

What am I trying to prove here? Of course max and min are commutative and "idempotent."

Exactly what kind of truth value are we computing here?
If we imagine that the truth value represents the bias of a Bernoulli random
variable, then taking the min of two truth values of two facts (a and b)
represents the bias of a fact which is true iff (a & b) if a and b are
maximally correlated. If a and b are independent, then the truth value should
be the product of the truth values instead.

The max of two truth values corresponds to the the truth value of (a | b), if a
and b are maximally correlated. If a and b are independent, then the correct
function to perform on the truth values (call them x and y) is 1 - (1 - x) * (1 - y).
If the last function is associative:
1 - (1 - (1 - (1 - x) * (1 - z))) * (1 - y) ?= 1 - (1 - x) * (1 - (1 - (1 - y) * (1 - z)))
Left side:
1 - (1 - (1 - (1 - x) * (1 - z))) * (1 - y)
1 - (1 - (1 - (1 - x - z + xz))) * (1 - y)
1 - (1 - (x + z - xz)) * (1 - y)
1 - (1 - x - z + xz) * (1 - y)
1 - ((1 - x - z + xz) - y(1 - x - z + xz))
1 - (1 - x - z + xz - y + yx + yz - yxz)
x + z - xz + y - yx - yz + yxz
Right side:
1 - (1 - (1 - (1 - y) * (1 - z))) * (1 - x)
1 - (1 - (1 - (1 - y - z + yz))) * (1 - x)
1 - (1 - (y + z - yz)) * (1 - x)
1 - (1 - y - z + yz) * (1 - x)
1 - ((1 - y - z + yz) - x(1 - y - z + yz))
1 - (1 - y - z + yz - x + xy + xz - xyz)
y + z - yz + x - xy - xz + xyz

Those four functions are all associatiive and commutative. Unfortunately, the
functions which correspond to minimal correlation are not idempotent. This
should not be surprising, since in the trivial case of combining the truth
value of a fact with itself, we should expect incorrect results if we assume a
the truth value is not correlated with itself.

In most Bayseian learning systems, the asssumption used to improve tractability
is that things are independent. In this case, we instead assume "maximum
correlation/dependence". Does that still work well?


We would like proof that weight optimization will lead to the "selection" of
correct rules from our program, given that the correct rules are present in our
program. By selection, I mean that we would like the weights for those rules to
become 1.0, or at least distinguishably higher than every other weight in the
program. If we continuously renormalize the weights, then all we actually need
to prove is that all wrong rules get eliminated. It seems obvious that this
should happen, so I guess I'll stop worrying about it.


One thing that's annoying is that we can't easily compute the fixed point of
the truth values of a program. For the min-max truth value, we can at least
guarantee that calling `evaluate_bottom_up` enough times will result in
convergence. However, error propagation is kinda a mess. And even for normal
evaluation, the number of iterations to reach convergence can be the length of
the longest loop in the fact graph (which can potentially be the entire
database). In comparison, loop detection in a list using the pointer following
algorithm guarantees detection in at most two passes through the loop.

Yeah, the update conditions upon finding a "wave collision" devolve into
keeping a tree copied from the part of the fact graph that lead to the current
fact. Instead, it should be cheaper (and more useful) to improve the dirtyness
tracking in the bottom up algorithm.

But what about error propagation? That can still diverge. Even if we
arbitrarily restrict the range of adjustments (to say 0.0 to 1.0), the values
can still oscillate if we don't restrict the weights to positive values. So
maybe do that?



As for error propagation:
We need to avoid looping infinitely. It would be nice if in doing so we
actually compute the correct error, instead of ignoring loops.  The simplest
way to avoid looping is to keep a set of already visited facts.  When a fact is
visited, do not add it to the frontier.  This technique completely fails for
diamonds in the fact graph, which is basically unacceptable.  Conceptually, the
best idea I have for how to make this actually work is the as follows.  For
every entry in the frontier, keep a mapping from facts to the weight that fact
had when it was visited on the way to adding this entry to the frontier, and
also the number of times that fact has been visited along this pass. When a
fact is first found in this mapping, compute a "loop closure adjustment, and
store it in a separate adjustment table. However, still propagate the error as
normal. The second time the fact is encountered from its own entailer table, do
not propagate it.  After normal propagation, use the loop closure adjustment
table to determine how weights diverge. Asjust them to the appropriate
divergent values, then propagate the error resulting from divergent weights.

Another perspective is that loops in the fact graph are essentially anomalous,
and not worth computing properly. In that case, the first time a fact is
encountered in its own entailer table, we should just drop it from propagation.

It works! In a minimal sense. In retrospect, I need to actually do the math on
how to optimize the network.

Damn, remember how to do back propagation correctly from first principles is a
pain.

We want to compute the derivative of the output w.r.t. the weights.

```
u[i, j] = ?
v[i, j] = both v[i - 1, j] u[i, j]
w[i, j] = finalize v[i, j]
x[i, j] = either x[i, j - 1] w[i, j]

u[i, j] = ?
v[i, j] = min v[i - 1, j] u[i, j]
w[i, j] = q * v[i, j]
x[i, j] = max x[i, j - 1] w[i, j]

du[i, j] = ?
dv[i, j] = argmin [v[i - 1, j] u[i, j]] [dv[i - 1, j], du[i, j]]
dw[i, j] = q * dv[i, j] + dq * v[i, j]
dx[i, j] = argmax [x[i, j - 1] w[i, j]] [dx[i, j - 1], dw[i, j]]

gx[i, j] = ?
gw[i, j] = argmax [x[i, j - 1], w[i, j]] [0, gx[i, j]]
gx[i, j - 1] += argmax [x[i, j - 1], w[i, j]] [gx[i, j], 0]
gv[i, j] += q * gw[i, j]
gq += v[i, j] * gw[i, j]
gu[i, j] = argmin [v[i - 1, j], u[i, j]] [0, gv[i, j]]
gv[i - 1, j] += argmin [v[i - 1, j], u[i, j]] [gv[i, j], 0]

gx[i, j] = ?
gx[i, j - 1], gw[i, j] = back_either x[i, j - 1], w[i, j], gx[i, j]
gq, gv[i, j] += back_finalize q, v[i, j], gw[i, j]
gv[i - 1, j], gu[i, j] += back_both v[i - 1, j], u[i, j], gv[i, j]

 = argmin [v[i - 1, j], u[i, j]] [0, gv[i, j]]

e = (1/2) * (y - x) ** 2
d e = y - x * (d y - d x) ???
e = (1/2) * (y**2 - 2 * y * x + x**2)
d e = (1/2) * (2 * y - 2 * (y * d x + d y * x) + 2 * x)
d e = y * d x + d y * x
gy = x * ge
gx = y * -ge
```

Intuition check:

(Minimizing error of a linear function.)
```
e = (1/2) * (w * x - y) ** 2

w = ?
x = ?
y = ?
a = w * x
b = a - y
d = b * b
e = d / 2

dw = ?
dx = ?
dy = ?
da = w * dx + dw * x
db = da - dy
dd = 2b * db
de = dd / 2

de = (2(w * x - y) * (w * dx + dw * x - dy)) / 2
de = (w * x - y) * (w * dx + dw * x - dy)
de/dw = (w * x - y) * x

ge = ?
gd = ge / 2
gb = 2 * gd
ga = gb
gy = -gb
gx = w * ga
gw = x * ga
```
