#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Proposition and logic",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Propositions
#definition(title: "Definition: Proposition")[
  A proposition is a statement that is either true or false.
]

#example(title: "Example: Propositions")[
  - $2+2=4$ is a proposition, and it is *true*.
  - $x+2=1$ is *not* a proposition, since its truth value depends on $x$.
  - $2+2=5$ is a proposition, and it is *false*.
]

Propositions are often abbreviated as T (True) or F (False).

#definition(title: "Notation: True and false")[
  In addition to T and F, the symbols $⊤$ (top) and $⊥$ (bottom) are sometimes used to denote *true* and *false* respectively. Both notations mean the same thing and may be used interchangeably throughout these notes.
]

= Logical operators
Logical operators combine propositions into compound propositions, allowing us to express more complex statements concisely.

The most important operators are:
- Negation: $not$ (not)
- Conjunction: $and$ (and)
- Disjunction: $or$ (or)
- Implication: $imp$ (implies)
- Bi-implication: $bi$
- Exclusive or: $xor$

*Operator precedence* (highest to lowest):
1. Negation: $not$ (not)
2. Conjunction: $and$ (and)
3. Disjunction: $or$ (or)
4. Implication: $imp$ (implies)
5. Bi-implication: $bi$

There is no universally agreed-upon precedence for exclusive or ($xor$).
#pagebreak()

= Truth tables
Truth tables define the operators exhaustively by listing every combination of truth values for $p, q$ etc. .

Truth tables in discrete mathematics have a number of rows equal to $2^n$, where $n$ is the 
number of variables.

#example(title: "Example: number of rows")[
  Given the proposition $p or q and r$

  Any truth table for this proposition must have $2^3 = 8$ rows

  #table(
  columns: 3,
  [$p$], [$q$], [$r$],
  [$top$], [$top$], [$top$],
  [$top$], [$top$], [$bot$],
  [$top$], [$bot$], [$top$],
  [$top$], [$bot$], [$bot$],
  [$bot$], [$top$], [$top$],
  [$bot$], [$top$], [$bot$],
  [$bot$], [$bot$], [$top$],
  [$bot$], [$bot$], [$bot$],
  )

]



== Conjunction
True only if both propositions are true.

#table(
  columns: 3,
  [$p$], [$q$], [$p and  q$],
  [$top$], [$top$], [$top$],
  [$top$], [$bot$], [$bot$],
  [$bot$], [$top$], [$bot$],
  [$bot$], [$bot$], [$bot$],
)

== Negation
The opposite of a proposition $p$.

#table(
  columns: 2,
  [$p$], [$not p$],
  [$top$], [$bot$],
  [$bot$], [$top$],
)

== Disjunction
True if at least one of the propositions is true.

#table(
  columns: 3,
  [$p$], [$q$], [$p or  q$],
  [$top$], [$top$], [$top$],
  [$top$], [$bot$], [$top$],
  [$bot$], [$top$], [$top$],
  [$bot$], [$bot$], [$bot$],
)


== Implication
False only when $p$ is true and $q$ is false.

#table(
  columns: 3,
  [$p$], [$q$], [$p to q$],
  [$top$], [$top$], [$top$],
  [$top$], [$bot$], [$bot$],
  [$bot$], [$top$], [$top$],
  [$bot$], [$bot$], [$top$],
)

== Bi-implication
True only if both propositions have the same truth value.

#table(
  columns: 3,
  [$p$], [$q$], [$p bii q$],
  [$top$], [$top$], [$top$],
  [$top$], [$bot$], [$bot$],
  [$bot$], [$top$], [$bot$],
  [$bot$], [$bot$], [$top$],
)

== Exclusive or
True only if the propositions have opposite truth values.

#table(
  columns: 3,
  [$p$], [$q$], [$p ⊕ q$],
  [$top$], [$top$], [$bot$],
  [$top$], [$bot$], [$top$],
  [$bot$], [$top$], [$top$],
  [$bot$], [$bot$], [$bot$],
)

#pagebreak()

= Tautology, contradiction, and contingency
#definition(title: "Definition: Tautology")[
  A proposition that is always true, no matter the truth values of its variables, is called a *tautology*.
]

#example(title: "Example: A tautology")[
  We want to show that $p or  (q to not (p and  q)) teq not (q and  not p)$.

  #table(
    columns: 8,
    [$p$], [$q$], [$p and  q$], [$not (p and  q)$], [$q to not (p and  q)$], [$p or  (q to not (p and  q))$], [$not p$], [$q and  not p$],
    [$top$], [$top$], [$top$], [$bot$], [$bot$], [$top$], [$bot$], [$bot$],
    [$top$], [$bot$], [$bot$], [$top$], [$top$], [$top$], [$bot$], [$bot$],
    [$bot$], [$top$], [$bot$], [$top$], [$top$], [$top$], [$top$], [$top$],
    [$bot$], [$bot$], [$bot$], [$top$], [$top$], [$top$], [$top$], [$bot$],
  )

  Comparing the two sides shows that $p or  (q to not (p and  q))$ is *always true* — it is a tautology.
]

#definition(title: "Definition: Contradiction")[
  A proposition that is always false, no matter the truth values of its variables, is called a *contradiction*. A contradiction is the opposite of a tautology.
]

#example(title: "Example: A contradiction")[
  $(p and  q) and  not (p or  q)$ is a contradiction:

  #table(
    columns: 3,
    [$p$], [$q$], [$(p and  q) and  not (p or  q)$],
    [$top$], [$top$], [$bot$],
    [$top$], [$bot$], [$bot$],
    [$bot$], [$top$], [$bot$],
    [$bot$], [$bot$], [$bot$],
  )
]
#pagebreak()

#definition(title: "Definition: Contingency")[
  A proposition that is neither a tautology nor a contradiction — i.e. it is true for some truth assignments and false for others — is called a *contingency*.
]

#example(title: "Example: A contingency")[
  $p and  q bii not q$ is a contingency:

  #table(
    columns: 4,
    [$p$], [$q$], [$p and  q$], [$p and  q bii not q$],
    [$top$], [$top$], [$top$], [$bot$],
    [$top$], [$bot$], [$bot$], [$top$],
    [$bot$], [$top$], [$bot$], [$bot$],
    [$bot$], [$bot$], [$bot$], [$top$],
  )

  Since the result is a mix of T and F, $p and  q bii not q$ is a contingency.
]

#pagebreak()

= Logical equivalence
Two propositions are *logically equivalent* if they have the same truth table. Tautology and logical equivalence are closely related, and equivalence is written with $teq$.

#definition(title: "Definition 1.3.2")[
  We call two propositions $s, t$ *logically equivalent*, written $s teq t$, if $s bii t$ is a tautology.
]

*Note:*
- In other words, $s$ and $t$ are two ways of saying the same thing.
- To find out whether $s teq t$, instead of constructing the truth table for $s bii t$, one can compare the truth tables for $s$ and $t$.
- The symbol $teq$ is not a logical operator, so $s teq t$ is not considered a compound proposition (while $s bii t$ is).

#example(title: "Example: p ⇒ q teq not p or  q")[
  #table(
    columns: 4,
    [$p$], [$q$], [$p to q$], [$not p or  q$],
    [$top$], [$top$], [$top$], [$top$],
    [$top$], [$bot$], [$bot$], [$bot$],
    [$bot$], [$top$], [$top$], [$top$],
    [$bot$], [$bot$], [$top$], [$top$],
  )

  The two columns match for every row, so $p to q teq not p or  q$.
]
#pagebreak()

There are many ways to find equivalences. A few useful ones are covered below, starting with the distributive laws.

#definition(title: "Distributive laws (Example 1.3.4)")[
  $ p or  (q and  r) teq (p or  q) and  (p or  r) $
  $ p and  (q or  r) teq (p and  q) or  (p and  r) $
]

#example(title: "Table 5: p or  (q and  r) and (p or  q) and  (p or  r) are logically equivalent")[
  #table(
    columns: 6,
    [$p$], [$q$], [$r$], [$q and  r$], [$p or  (q and  r)$], [$(p or  q) and  (p or  r)$],
    [$top$], [$top$], [$top$], [$top$], [$top$], [$top$],
    [$top$], [$top$], [$bot$], [$bot$], [$top$], [$top$],
    [$top$], [$bot$], [$top$], [$bot$], [$top$], [$top$],
    [$top$], [$bot$], [$bot$], [$bot$], [$top$], [$top$],
    [$bot$], [$top$], [$top$], [$top$], [$top$], [$top$],
    [$bot$], [$top$], [$bot$], [$bot$], [$bot$], [$bot$],
    [$bot$], [$bot$], [$top$], [$bot$], [$bot$], [$bot$],
    [$bot$], [$bot$], [$bot$], [$bot$], [$bot$], [$bot$],
  )
]

The distributive law only works with Or and And — the method is to build the truth table for both sides and compare the whole way through.

#pagebreak()

== Truth tables for equivalence and contraposition

#definition(title: "Contraposition (Table 1.3.7, line 2)")[
  $ p to q teq not q to not p $

  *Intuition:*
  - If $q$ is F, then $p to q$ only becomes T if $p$ is F.
  - This is what $not q to not p$ states.

  *Proof:*
  #table(
    columns: 5,
    [$p$], [$q$], [$p to q$], [$not q$], [$not p$], [$not q to not p$],
    [$top$], [$top$], [$top$], [$bot$], [$bot$], [$top$],
    [$top$], [$bot$], [$bot$], [$top$], [$bot$], [$bot$],
    [$bot$], [$top$], [$top$], [$bot$], [$top$], [$top$],
    [$bot$], [$bot$], [$top$], [$top$], [$top$], [$top$],
  )
]

#definition(title: "Equivalences involving implications (2): formulation using and , or , not  (Table 1.3.7, line 1)")[
  $ p to q teq not p or  q $

  *Intuition:*
  - If $p$ is F, both propositions are T.
  - If $p$ is T, for either proposition to be T, $q$ must be T.

  *Proof:*
  #table(
    columns: 5,
    [$p$], [$q$], [$p to q$], [$not p$], [$not p or  q$],
    [$top$], [$top$], [$top$], [$bot$], [$top$],
    [$top$], [$bot$], [$bot$], [$bot$], [$bot$],
    [$bot$], [$top$], [$top$], [$top$], [$top$],
    [$bot$], [$bot$], [$top$], [$top$], [$top$],
  )
]
#pagebreak()

#definition(title: "Equivalences involving implications (3): the implication and the bi-implication (Table 1.3.8, line 1)")[
  $ (p to q) and  (q to p) teq p bii q $

  *Intuition:* The rewritten left-hand side $(p to q) and  (not p to not q)$ means both $p$ and $q$ need to have the same truth value.

  *Proof:*
  #table(
    columns: 6,
    [$p$], [$q$], [$p to q$], [$not p to not q$], [$(p to q) and  (not p to not q)$], [$p bii q$],
    [$top$], [$top$], [$top$], [$top$], [$top$], [$top$],
    [$top$], [$bot$], [$bot$], [$top$], [$bot$], [$bot$],
    [$bot$], [$top$], [$top$], [$bot$], [$bot$], [$bot$],
    [$bot$], [$bot$], [$top$], [$top$], [$top$], [$top$],
  )

  *Note:* This justifies the notation of $bi$ and saying "$p$ if and only if $q$".
]

#pagebreak()

== Sets of numbers
#definition(title: "Definition: Some sets of numbers")[
  Important sets of numbers used in this and later lectures:
  - $ZZ = {dots, -2, -1, 0, 1, 2, dots}$ is the set of *integers*.
  - $ZZ^+ = {1, 2, 3, dots}$ is the set of *positive integers*.
  - $ZZ^- = {dots, -3, -2, -1}$ is the set of *negative integers*.
  - $NN = {0, 1, 2, 3, dots}$ is the set of *natural numbers*. In some sources this starts at $1$.
  - $QQ = {p/q | p,q in ZZ, q ≠ 0}$ is the set of *rational numbers*, i.e. numbers given by any non-empty finite sequence of digits before the comma (possibly just $0$) and any sequence of digits after the comma (possibly the empty sequence).
  - $RR$ is the set of *real numbers*.
  - $∅$ is the *empty set* (contains no members).
]

== Open propositional functions
#definition(title: "Definition: Open propositional function")[
  An open proposition (propositional function) is a statement that contains one (or more) variables, usually named something like $P(x)$.

  *Remarks:*
  - The variables usually represent numbers.
  - When the variables are replaced with actual values, one obtains a proposition.
  - For now, we will only focus on open propositions with a single variable.
]
#pagebreak()

#example(title: "Example: Evaluating a propositional function")[
  We define a propositional function $P(x) teq 2x > x$.

  $ P(-1) teq -2 > -1 teq bot $
  $ P(0) teq 0 > 0 teq bot $
  $ P(1) teq 2 > 1 teq top $
  $ P(2) teq 4 > 2 teq top $
]
= Quantifiers
*Tips for quantifiers:*
- There are three quantifiers: $∀ x$, $∃ x$, $∃! x$.
- $D$ stands for *domain*, i.e. the universe the quantifier ranges over.
- Quantifiers are placed before logical operators.
- The order of nested quantifiers matters quite a lot.

== Universal quantifier
For a given propositional statement, one can say something about *all* its variables. The universal quantifier says something is true for *every* $x$ in the domain, and is read as:

$ ∀ x in D : P(x) $ "For all $x$ in the domain $D$, $P(x)$"

#definition(title: "Definition 1.4.1")[
  For a propositional function $P(x)$, the statement
  $ ∀ x in D : P(x) $
  is equivalent to the statement that $P(x)$ is true for all $x$ in the set $D$. We call $∀$ the *universal quantifier*.
]

#example(title: "Example: Universal quantifier")[
  $ ∀ x in ZZ : 2x > x, $ so it matters what $D$ is!

  $ ∀ x in ∅ : 2x > x teq top $

  Let's say you have never driven a Ferrari. Then "whenever I have driven a Ferrari, I have crashed it" is *true*!

  *Comments:*
  1. All $x$ belonging to the real numbers, so $2x > x$ (False).
  2. All $x$ belonging to the empty set, so $2x > x$ (True).
]
#pagebreak()

== Existential quantifier
$ ∃ x in D : P(x) $ is read as "there exists at least one $x$ in the domain such that $P(x)$ is true".

$ ∃ x in ∅ : P(x) teq bot $

#definition(title: "Definition 1.4.2")[
  For a propositional function $P(x)$, the statement
  $ ∃ x in D : P(x) $
  is equivalent to the statement that there exists at least one $x$ in the set $D$ such that $P(x)$ is true. We call $∃$ the *existential quantifier*.

  *Remarks:*
  - Read: "there exists $x$ in $D$ such that $P(x)$ is true" or "for some $x$ in $D$, $P(x)$".
  - An existential quantification over the empty set is always false.
  - The existential quantification is true as long as there exists at least one $x$ with the specified property, not just precisely one.
]

#example(title: "Example: Existential quantifier")[
  Let $Q(x) teq x^2 = 4$.

  Then $Q(x)$ can be unfolded as the (infinite) disjunction $Q(1) or  Q(2) or  Q(3) or  dots$, which is true as soon as *one* disjunct is true.

  $ ∃ x in ZZ : Q(x) teq top $

  since $Q(2) teq 2^2 = 4 teq top$ — we only need *one* $x$ making the proposition true, not all of them.
]
#pagebreak()

== Uniqueness quantifier
$ ∃! x in D : P(x) $ "There exists precisely one $x$ in the domain such that $P(x)$ is true"

#definition(title: "Definition: Uniqueness quantifier")[
  For a propositional function $P(x)$, the statement
  $ ∃! x in D : P(x) $
  is equivalent to the statement that there exists precisely one $x$ in the set $D$ such that $P(x)$ is true. We sometimes call $∃!$ the *uniqueness quantifier*.

  *Remarks:*
  - Read: "there exists precisely one $x$ in $D$ such that $P(x)$ (or "$P(x)$" is true)".
]

#example(title: "Example: Uniqueness quantifier")[
  Let $Q(x) teq x^2 = 4$.

  Over $D = ZZ$, both $x = 2$ and $x = -2$ satisfy $Q(x)$, so there is *more than one* solution:
  $ ∃! x in ZZ : Q(x) teq bot $

  But over $D = ZZ^+$, only $x = 2$ satisfies $Q(x)$:
  $ ∃! x in ZZ^+ : Q(x) teq top $
]
#pagebreak()

== Quantifiers with operators
*(How do you read quantifiers?)*

*Method:* when you get an assignment with quantifiers and operators, proceed as follows:

$ ∀ x in ZZ : x <= 3 <-> 4 > 5 teq (∀ x in ZZ : x <= 3) and  (4 > 5) teq top $

1. *Read the domain and the quantifier.* "For all $x$'s that belong to the integers, so $x <= 3 <-> 4 > 5$, and true."

   Now we only have one $·<->·$ quantifier, so first, what does the sign say when isolated?

   For all $x$'s to belong to Z (the integers), so it must hold that $x <= 3$. Here is $x <= 3$ the part inside Z (integers), so the whole thing either holds or does not.

2. *Take the proposition and fill in the numbers* — first $3$. You can then ask yourself "For all $x$'s that belong to Z, so it is true that $x <= 3$. Does that hold or not?"

3. *Many people stop here for the next proposition.* The bar is just "$4 > 5$" which is obviously false.

4. *Look at the operator from the Truth Table Bi-implication.* iff both propositions have the same truth value. Since neither proposition holds, the whole thing is true.
#pagebreak()

== Restricted domains in quantifiers
This is about restricting the domain, so you add $x$ (as input) belonging to $D$, telling something on the other side of the propositional function.

#definition(title: "Definition: Restricted domains")[
  For $x in D$, $Q(x)$: $P(x) teq ∀ x in D : (Q(x) to P(x))$

  Meaning: for all $x$'s that belong in $Q(x)$, so is it the case that $Q(x)$ implies $P(x)$ (because $P(x)$ and logically equivalent with $Q(x)$)

  - $∀ x in D, Q(x) : P(x) teq ∀ x in D : (Q(x) to P(x))$,
  - $∃ x in D, Q(x) : P(x) teq ∃ x in D : (Q(x) and  P(x))$,
  - $∃! x in D, Q(x) : P(x) teq ∃! x in D : (Q(x) and  P(x))$.
]

#example(title: "Example: Restricted domains")[
  Let $Q(x) teq 2x > x + 4$.

  Then $Q(5) and  Q(6) and  Q(7) and  dots teq top$

  $ teq ∀ x in {5, 6, 7, dots} : Q(x) $
  $ teq ∀ x in ZZ, x >= 5 : Q(x) $
  $ teq ∀ x in ZZ : (x > 5 to Q(x)) $

  $ ∃ x in ZZ, x <= 4 : Q(x) teq ∃ x in ZZ : (x <= 4 and  Q(x)) $
]

#pagebreak()

== Nested Quantifiers with multiple variables
One can use multiple quantifiers when having more variables
= Good to know equivalences

#theorem(title: "Table 6: Logical equivalences")[
  #table(
    columns: 2,
    [*Equivalence*], [*Name*],
    [$p and  top teq p$ \ $p or  bot teq p$], [Identity laws],
    [$p or  top teq top$ \ $p and  bot teq bot$], [Domination laws],
    [$p or  p teq p$ \ $p and  p teq p$], [Idempotent laws],
    [$not (not p) teq p$], [Double negation law],
    [$p or  q teq q or  p$ \ $p and q teq q and  p$], [Commutative laws],
    [$(p or  q) or  r teq p or  (q or  r)$ \ $(p and  q) and  r teq p and  (q and  r)$], [Associative laws],
    [$p or  (q and  r) teq (p or  q) and  (p or  r)$ \ $p and  (q or  r) teq (p and  q) or  (p and  r)$], [Distributive laws],
    [$not (p and  q) teq not p or  not q$ \ $not (p or  q) teq not p and  not q$], [De Morgan's laws],
    [$p or  (p and  q) teq p$ \ $p and  (p or  q) teq p$], [Absorption laws],
    [$p or  not p teq top$ \ $p and  not p teq bot$], [Negation laws],
  )
]
#pagebreak()

#theorem(title: "Table 7: Logical equivalences involving conditional statements")[
  $ p to q teq not p or  q $
  $ p to q teq not q to not p $
  $ p or  q teq not p to q $
  $ p and  q teq not (p to not q) $
  $ not (p to q) teq p and  not q $
  $ (p to q) and  (p to r) teq p to (q and  r) $
  $ (p to r) and  (q to r) teq (p or  q) to r $
  $ (p to q) or  (p to r) teq p to (q or  r) $
  $ (p to r) or  (q to r) teq (p and  q) to r $
]

#theorem(title: "Table 8: Logical equivalences involving biconditional statements")[
  $ p bii q teq (p to q) and  (q to p) $
  $ p bii q teq not p bii not q $
  $ p bii q teq (p and  q) or  (not p and  not q) $
  $ not (p bii q) teq p bii not q $
]
#pagebreak()

= Satisfiability
#definition(title: "1.3.5 Satisfiability")[
  A compound proposition is *satisfiable* if there is an assignment of truth values to its variables that makes it true (that is, when it is a tautology or a contingency). When no such assignments exists, that is, when the compound proposition is false for all assignments of truth values to its variables, the compound proposition is *unsatisfiable*. Note that a compound proposition is unsatisfiable if and only if its negation is true for all assignments of truth values to the variables, that is, if and only if its negation is a tautology.

  When we find a particular assignment of truth values that makes a compound proposition true, we have shown that it is satisfiable; such an assignment is called a *solution* of this particular satisfiability problem. However, to show that a compound proposition is unsatisfiable, we need to show that *every* assignment of truth values to its variables makes it false. Although we can always use a truth table to determine whether a compound proposition is satisfiable, it is often more efficient not to, as Example 9 demonstrates.
]

Some exercises will ask you to determine the truth value from a *given* set of values for a proposition. Remember that talking about satisfiability really just means asking: "Can this statement be true?" If there is a way to make a statement true, we say that it is *satisfiable*.

#example(title: "Example: Determining satisfiability from given values")[
  (a) If $p$ is true and $q$ is false, then $p and  q$ is false.

  Even if it wasn't stated on the slides, we can use the satisfiability argument: we simply assume the truth values of $p$ and $q$ and evaluate the operator.

  $ p = top, q = bot $
  $ p and  q = top and  bot = bot $

  From the question we were asked, we can conclude that this is correct.
]

Recall from the CS (Datalogi) course:

#definition(title: "Satisfiability (Boolean expressions)")[
  A Boolean expression with $n$ variables $x_1, x_2, dots, x_n$ is called *satisfiable* if there exists a set of values (e.g. $x_1 = bot, x_2 = top, dots, x_n = bot$) that makes the whole expression true.

  I.e. if there exists a row in the expression's truth table where the output is $1$.

  *The SAT problem:* given a Boolean expression with $n$ variables, decide whether it is satisfiable (and if so, find a satisfying set of values for $x_1, x_2, dots, x_n$).
]

