import autograd as auto
import autograd.numpy as np

def f(x):
    return (1-x[0])**2 + 100*(x[1]-x[0]**2)**2

nabla_f = auto.grad(f)

x0 = np.array([-1.2, 1])
d  = -(nabla_f(x0))

def backtracking_line_search(f, grad, x, d, alpha_0=1, p=0.5, beta=1e-4):
    y, g, alpha = f(x), grad(x), alpha_0
    while ( f(x + alpha * d) > y + beta * alpha * np.dot(g, d) ) :
        alpha *= p
    return alpha

def solve(f, nabla_f, x, d, max_iter=1000):
    for k in range(max_iter):
        g = nabla_f(x)
        if np.linalg.norm(g) < 1e-6:   # convergence check
            print(f"Converged at iteration {k}")
            break
        d = -g
        alpha = backtracking_line_search(f, nabla_f, x, d)
        print(f"iter {k}: alpha = {alpha:.6f},  f(x) = {f(x):.6f}")
    x = x + alpha * d
    return x


x = solve(f,nabla_f,x0,d)

print(x)

print("done!")