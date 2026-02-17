data Nat = Zero | Succ Nat

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)