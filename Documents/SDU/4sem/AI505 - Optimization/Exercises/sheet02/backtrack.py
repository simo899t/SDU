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

alpha = backtracking_line_search(f,nabla_f,x0,d)

print(alpha)

print("done!")