Gradient Vector
$$nabla_S f(x) equiv underbrace(lim_(h->0) (f(x+h s)-f(x))/h, "forward difference") = underbrace(lim_(h->0) (f(x+(h s)/2)-f(x-(h s)/2))/h, "central difference") = underbrace(lim_(h->0) (f(x)-f(x-h s))/h, "backward difference")$$
To compute $nabla_s f(x)$:
- Compute $$nabla_s f(x) = (diff f)/(diff x_1)s_1 +(diff f)/(diff x_2)s_2+ dots + (diff f)/(diff x_n)s_n =nabla f(x)^T S = nabla f(x) dot S $$
#### Matrix Calculus
$$nabla_x b^T x = nabla_x x^T b = b$$
$$nabla_x x^T A x = (A+A^T)x$$
#### Positive definteness
![[Pasted image 20260210104115.png]]
#### LU Decomposition
For $$P A = L U$$ Where $L$ is a *lower triangular* matrix, $U$ is an *upper triangular* matrix and $P$ is a *permutation matrix* (obtained by rearranging the rows of $A$)
- Use LAPACK (FORTRAN library for python)