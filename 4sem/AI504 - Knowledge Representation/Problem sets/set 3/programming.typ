#import "temp/temp.typ": *

#show: assignment.with(
  title: "Problem set 3 - A decider for truth in a model for " + $cal(A)$,
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj", "Daniel Nissen"),
  date: "March, 2026",
  outline-depth: 1
)

= Problem 1: The type of sentences <sec1>
Define a unary type constructor Sentence following Definition 1.1. If $p$ is a type, then Sentence $p$ must be a type representing all possible sentences of logic $cal(A)$ using p as a signature. In particular, if `a :: p ` and ` b :: p`, then there should be some expression `AllAre a b :: p`. Only expressions 
constructed using such a rule should have type Sentence p.1


_Example 1.1_:  Let Char be the set of all Unicode code points. Then:
$ phi := "All" 'p' "are" 'q' $
is a sentence over signature Char.


```hs
phi :: Sentence Char
phi AllAre 'p' 'q'
```
== Solution 
We want to make a type constructor. That is `data typeName type = arguments`

```hs
data Sentence p = AllAre p p
```

We define `Sentence` taking the type `p`, and argues that `AllAre p p` (All p are p, where p is just a placeholder). 
This type constructors ensures that if there exists ```hs a :: p``` and ```hs b :: p```, 
then \ ```hs AllAre a b :: Sentence p```

#pagebreak()

= Problem 2: The type of models
Define a binary type constructor `Model` following Definition 1.3. If `p` and `m` are types, then `Model p m` must be a type representing all possible models for logic $cal(A)$ with signature p and universe m. Let f be  a function whose domain is p and whose codomain is the type of all finite2 sets of terms in m, then  there should be some expression `Mod f :: Model p m`. Only expressions constructed using such a  rule should have type `Model p m`. Search the imported library for the “type of finite sets of  expressions of a certain type”.

Note that terms of Model p m only need to specify the interpretation function $[||]$ (named f above) from the pair $model = [M, [| |]$ as described in Definition 1.3. This is because the universe $M$ is already  present in the type as `Model p m` as `m`.

_Example 2.1_:  Let Bool and String be the set of all boolean values and countable Unicode  strings respectively. We use Bool as signature. Then:

#align($ 
model_0 &:= ("String", [| |]_0: "Bool" -> pow("String"))\
forall b &in "Bool".quad [|b|]_0 := emptyset\
model_1 &= (ZZ, [| |]_1: ZZ -> pow(ZZ))\
forall z &in ZZ. quad [|z|]_1 := {-abs(z), dots abs(z)}
$)

```hs
m0 :: Model Bool String
m0 = Mod (const empty)

m1 :: Model Integer Integer
m1 = Mod (\ z -> fromList [- abs z .. abs z])
```


== Solution
We want to make another type constructor much like in @sec1. This time it should define the $model$ as $ model = (P, [||]:P->pow(M)) $

For a Haskell implementation wee need a constructor tha takes both a signature #hs("p") and a univere (domain) #hs("m"). It should then use the #hs("Mod f"), to map the signatures to a some set (usually a subset of the domain)

```hs
data Model p m = Mod (p -> Set m)
```


#pagebreak()

= Problem 3: A decider for $ent$
Let $U$ be some set with a subset $Y psubset U$. We can call $(U,Y)$ a decision problem, and every element of $U$ an instance of decision problem $U$. The elements in $Y$  are yes-instances, i.e. instances for which the correct answer is “Yes!”. The elements in $U without Y$ are no-instances, i.e. instances for which the correct answer is “No!”. A decider for $(U,Y)$ is a computer program which accepts elements of $U$ as 
inputs. It must terminate and return True when the input is in $Y$ , while it must terminate and return 

False when the input is not in $Y$ .

Fix a signature $P$ and a univers $M$. Consider the decision problem $(U,Y)$ where:
#align($
U:=& {(model,phi) | model "is a models and" phi "is a formula"}\
Y:=& {(model,phi) in U | model ent phi}
$)

Define an infix binary operator $ent$ which works as a decider for this decision problem. This operator 
is polymorphic in a generic type p representing $P$, and in a type m implementing the `Ord` type $"class"^3$ 
and representing $M$. The operator should be curried: the two expressions representing $model$ and $phi$ 
must be passed as the left and right arguments respectively.

_Example 3.1_:  Consider the previous example again, and remember that the Haskell Language Server #u("can evaluate snippets prefixed with >>> in comments") (unlike in Python, no need to use an interpreter!).

#align($ 
model_0 ent "All True are False" &=^"def" [|"True"|]_0 psubset [|"False"|]_0\
                                 &=^"def" emptyset psubset emptyset\
model_0 ent "All True are False" &=^"def" [|-3|]_1 psubset [|2|]_1\
                                 &=^"def" {-3,dots,3} psubset {-2,dots,2}
$)

The first statement holds, but the second does not.

```hs
-- >>> m0 |= AllAre True False
-- True
-- >>> m1 |= AllAre (-3) 2
-- False
```

Note that implementing a decider means implementing a function that returns a truth value. This is very different from an implementation of the truth predicate in Definition 1.4, which would return  an object representing the mathematical statement $model ent phi$, rather than its mere truth value.


== Solution
  ```hs
(|=) :: (Ord m) => Model p m -> Sentence p -> Bool
Mod f |= AllAre p q = f p `isSubsetOf` f q
  ```
Definign that $ model ent all(a,b) => [|a|] psubset [|b|] $

= Appendix
  ```hs
import Data.Set

-- Problem 1: A decider for

data Sentence p = AllAre p p

-- Problem 2: The type of models

data Model p m = Mod (p -> Set m)

-- Problem 3: A decider for 'entails'

(|=) :: (Ord m) => Model p m -> Sentence p -> Bool
Mod f |= AllAre p q = f p `isSubsetOf` f q

m0 :: Model Bool String
m0 = Mod (const empty)

m1 :: Model Integer Integer
m1 = Mod (\z -> fromList [-abs z .. abs z])

-- >>> m1 |= AllAre (-3) 2
-- False

-- >>> m0 |= AllAre True False
-- True

  ```