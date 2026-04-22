$[a, b] = {x ∈ R | a ≤ x ≤ b}$ closed interval 
$(a, b) = {x ∈ R | a < x < b}$ open interval

Linear combination
- with $v_1,v_2 dots, v_k in RR^n$
- and $lambda = [lambda_1,lambda_2dots,lambda_k]^T in RR^k$
$$x = lambda_1v_1 + dots + lambda_k v_k = sum_(i=1)^k lambda_i v_i$$
- Conic combination
- Affine combination
- Convex combination
#### Convex set
If $x,y in S$ and $0<=lambda<=1$ then $lambda x+(1-lambda)y in S$
- This means that for a set and any two point $x,y$ all point in between these points must be inside the set
![[Pasted image 20260205093114.png]]

#### Convex functions


If for any two points $forall x,y in R^n$ with $alpha in [0,1]$ it holds that
$$f(alpha x+(1-alpha)y)>= alpha f(x)+(1-alpha)f(y)$$
![[Pasted image 20260205093828.png]]
In this example grapf is convex on some interval while concave (opposite of convex) in some other interval

#### Hulls
- $"lin"(S)$ Linear hull (span)
- $"cone"(S)$ conic hull
- $"aff"(S)$ affine hull
- $"conv"(S)$ convex hull
	- convex hull are the points sorrounding all points (much like a rubber band)
![[Pasted image 20260205094451.png]]
$$"conv"(X) = {lambda_1 x_1+dots+lambda_n x_n | x_i in X, lambda_1, dots,lambda_n >=0 "and" sum_i lambda_1 = 1}$$
