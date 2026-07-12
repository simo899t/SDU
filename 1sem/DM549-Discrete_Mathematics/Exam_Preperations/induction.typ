#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Induction",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)
= Induction (weak)

Proof technique used to establish that a statement $P(n)$ holds for all integers $n>=m$. this involves 2 primary steps

1. Base case:
  
  + Prove that $P(m)$ holds
3. Inductive step:
  
  + Prove that $P(k) -> P(kplus) space forall k >= m$


== Applying Induction
1. *Understand* the statement
  
  + Identify $P(n)$, the proposition you are trying to prove.
2. Prove the *Base case*

  + Substitute $n = m$ into $P(n)$
  + Verify that $P(m)$ is true.



3. State an *Inductive hypothesis*

  + Assume $P(k)$ holds for some arbitrary $k>=m$. This is your inductive hypothesis.
4. Prove the *Inductive step*


  + Use the inductive hypothesis ($IH$) to show that $P(k) iimp P(kplus)$ 
  + Explicitly demonstrate where $P(k)$ is used in your   reasoning



5. *Conclude* the proof

  + State that $P(n)$ thereby holds for all $n>=m$, based on the base case and the inductive step

== Common pitfalls in weak induction proofs
- Overlooking conditions in the statement (e.g., $n>=m$).
- Forgetting to verify the base case.
- Not clearly showing how $P(k)$ leads to $P(kplus)$

#pagebreak()

#example(title: "Example: Proof by weak induction on sums")[
  #pseudo[
  *Proof by induction*

  - Goal: Prove $P(n)$ that 
  - $ summ(n,i=1,2^i) = 2^nplus -1 space forall n >=0 $
  + *$underline("Base case")$*
    + $2^0 = 2^(0+1)-1 = 1 quad top$ 
  + *$underline("Inductive hypothesis")$*
    + Assume that $summ(k,i=1,2^i) = 2^kplus - 1 space "for" k >=0$ (base case)
  + *$underline("Inductive step")$*
    + $ summ(kplus, i=0,2^i) &= (summ(k, i=0,2^i)) + 2^kplus qquad (IH" in parentheses") \ 
      &= (2^kplus - 1) + 2^kplus \ &= 2^(k+2) - 1 $ #QED
  ]

  Since base case is true, and we have shown that for any $P(k) iimp P(kplus)$,

  We have proven by induction that $summ(n,i=1,2^i) = 2^nplus -1 space forall n >=0$
  
]
#pagebreak()

= Strong induction
Strong induction extends the weak induction by the fact that some inductive steps might need to look back more than 1 step (e.g. Fibonacci).

This follows two similar steps

Given $l >= 0$ (how many extra steps back (beyond k) the inductive step needs.)

1. *Base case*

  + Prove that $P(i)$ holds for $m <= i <= m + l$

2. *Inductive step*
  + Prove that $ and.big_(1=k)^k P(i) -> P(k+1) space forall k>=m+l. $

== Common pitfalls in strong induction proofs
- Forgetting to verify all $l+1$ base cases if multiple are needed.
- Failing to explicitly demonstrate where the strong inductive hypothesis is used in the proof of $P(kplus)$
- Failing to incorporate all the needed $l+1$ base cases into the proof of $P(n)$


== Applying strong induction
1. *Understand* the statement

  + Identify $P(n)$, the proposition you are trying to prove.
  + If possible, determine the number of look-back steps $l$ needed by the inductive step.

2. Prove the *Base case*

  + Substitute $n = i$ into $P(n) "for all" i "where" m <= i <= m+l $
  + Verify that $P(m), P(m+1), dots, P(m+l)$ is true.

3. State a *Strong inductive hypothesis*

  + Assume $P(i)$ holds $forall i$ where $k-l<=i <=k$. This is your inductive hypothesis.
4. Prove the *Inductive step*


  + Use the inductive hypothesis ($IH$) to show that 
    $ and.big_(i = k-l)^k P(i) iimp P(kplus) space forall k where k >= m+l $
  + Explicitly demonstrate where the strong inductive hypothesis is used in your reasoning.


#pagebreak()

5. *Conclude* the proof

  + State that $P(n)$ thereby holds for all $n>=m$, based on the base case and the inductive step.
  + Demonstrate how the needed $l+1$ base cases are used to validate $P(n)$

#example(title: "Example: Proof by strong induction on integers")[
  #pseudo[
  *Proof by induction*
  - Prove that for every $n in NN$ with $n>=4$, there exists $a,b in NN$ such that $n = 2a+5b$
  - Since if $n = 2a + 5b$, then $n + 2 = 2(a+1) + 5b$, the inductive step increases $n$ by $2$.
  - To cover all integers $>= 4$, we therefore need two consecutive base cases ($l = 1$).
  + *$underline("Base case")$*
    + Verify for $n = 4:$ Let $a = 2$, $b=0$. Then $n = 2 times 2 + 5 times 0 = 4 quad top$
    + Verify for $n = 5:$ Let $a = 0$, $b=1$. Then $n = 0 times 2 + 5 times 1 = 5 quad top$
  + *$underline("Strong inductive hypothesis")$*
    + Assume that for all integers $i$ with $k-1 <= i <= k$ and $k >= 5$, there exist $a,b in NN$ such that $i = 2a + 5b$.
  + *$underline("Inductive step")$*
    + By $IH$ we have that $kmin = 2a+5b$ for some $a,b in NN$
    + Then
    + $ k+1 = (k-1) + 2 = 2(a+1) + 5b $
    + Since $a+1 in NN$ and $b in NN$, $k+1$ is in the required form. #QED
  ]
  By demonstrating that $k+1$ can be expressed as $(k-1) + 2 = 2(a+1)+5b$.

  Thus using strong induction as verified $P(4)$ and $P(5)$, we have shown that $P(n)$ holds $forall n in NN$ with $n >= 4$.
]

#pagebreak()

= Structural induction
Structural induction is a proof technique specifically for recursively defined structured. These structures can be trees, lists or grammars etc. Much like regular kinds of inductions it involves two steps.

1. *Base case*

  + Prove that $P(S_1)$ holds for the initial structure.

2. *Inductive step*
  + Prove that $P(S_1) iimp P(S_iplus)$ for all $i >= 1$, using a structural inductive hypothesis

== Common pitfalls in structural induction proofs
- Forgetting to verify the base case.
- Not demonstrating how the structural inductive hypothesis is applied
- Failing to cover all the recursive rules (if e.g. there are several rules)

== Applying structural induction
1. *Understand* the _recursive_ definition of the structure

  + Identify the recursive rules defining the structure
  + State the property $P(s)$ to prove for all structures $S$.

2. Prove the *Base case*

  + Verify that $P(S)$ holds for the initial structure.

3. State an *Structural inductive hypothesis*
  
  + Assume $P(S_i)$ holds for all $i>=1$.
4. Prove the *Inductive step*
   
  + Use the inductive hypothesis ($IH$) to prove that $P(S_iplus)$ holds for the derived structure $S_iplus$
  + Explicitly demonstrate where the structural inductive hypothesis is used in your reasoning.
5. *Conclude* the proof
  #set enum(numbering: "a)")
  + State that $P(S_i)$ holds for all structures defined by recursive ruled based on the verified basis step and the structural inductive step. 
#pagebreak()

#example(title: "Example: Proof by structural induction on a recursive set")[
  Given the set $3 in S$, which is defined recursively as follows
  $ quad  x,y in S iimp x + y in S $

  This defines a recursive  $S = {3,6,9,12,15,dots}$

  Prove that this recursion is the same as $S^prime = {3n| n in ZZ^+}$

  This is true iff $S subset S^prime$ and $S^prime subset S$

  So we can define $ P(k) = Q(k) and Q^prime (k) $

  Where $ Q(k) = (S_k subset S^prime)) "and" Q^prime (k) = (S^prime_k subset S)) $


  #pseudo[
  *Proof by induction*
  - Prove that $P(k) space forall i in ZZ^+$
  + *$underline("Base case")$*
    + For $P(1) "where" S_1 = {3}$ and $S^prime = {3}$ we can simplify since $S_1 = S_1^prime$ 
    + $ P(i) = Q(1) and Q^prime (1) = underbrace((S_1 subset S_1^prime) and (S_1^prime subset S_1), "trivial since" {3} = {3}) $
    + This is true.
  + *$underline("Inductive hypothesis")$*
    + Assume for some $k>=1$ that 
    + $ P(k) "is true" $
    + This means that
    + $ Q(k) and Q^prime (k) "is both true" $
  + *$underline("Inductive step")$*
    + Part 1
    + Given any $z in S_kplus$ and the recursive definition
    + $ S_kplus = S_k cup {x+y | x,y in S_k} $
    + We then have two cases:
      + Case 1 $Q(k)$
        + Let $z in S_k$, then by #IH since
        + $ S_k &subset  S^prime $
        + $ z in S_k iimp z &in S^prime $
        - #v(3em)
      + Case 2 $Q^prime (k)$
        + Let any $z = x + y$ where $x,y in S_k$
        + For $x$ and $y$ to exist there must be $a,b in ZZ^+$ such that 
        + $ z = 3a + 3b = 3(a+b) $
        + Since $a,b in ZZ^+$ then by #IH, $ z in P^prime iimp S_kplus subset S^prime$
    + Part 2
      + By definition $S^prime_kplus = {3(kplus)}$
      + Since $3(k+1) = 3k + 3$ and
      +  $ 3k in S "and" 3 in S $
      +  Then $ 3(kplus) in S iimp S^prime_kplus subset S  $
  ] 
  Thus by the basis step and the structural induction, it is proven that for any $k >= 0$ it holds that $S^prime subset S$ and $S subset S^prime$
]
