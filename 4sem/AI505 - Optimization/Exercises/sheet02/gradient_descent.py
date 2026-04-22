import autograd as auto
import autograd.numpy as np

def f(x):
    return 1/2 * x**2

nabla_f = auto.grad(f)

x0 = 1.0  # changed from 0 to avoid trivial starting point

def gradient_descent(f, grad, x0, alpha=0.1, tol=1e-6, max_iter=1000):
    x = x0
    for i in range(max_iter):
        g = grad(x)
        x_new = x - alpha * g
        if abs(x_new - x) < tol:
            print(f"Converged at iteration {i+1}")
            break
        x = x_new
    return alpha, x

alpha1, x_opt = gradient_descent(f, nabla_f, x0, 0.1)
print(f"With alpha: {alpha1}, optimal x: {x_opt:.6f}, f(x): {f(x_opt):.6f}")

alpha2, x_opt = gradient_descent(f, nabla_f, x0, 0.5)
print(f"With alpha: {alpha2}, optimal x: {x_opt:.6f}, f(x): {f(x_opt):.6f}")

alpha3, x_opt = gradient_descent(f, nabla_f, x0, 1)
print(f"With alpha: {alpha3}, optimal x: {x_opt:.6f}, f(x): {f(x_opt):.6f}")

alpha4, x_opt = gradient_descent(f, nabla_f, x0, 1.5)
print(f"With alpha: {alpha4}, optimal x: {x_opt:.6f}, f(x): {f(x_opt):.6f}")

alpha5, x_opt = gradient_descent(f, nabla_f, x0, 1.9)
print(f"With alpha: {alpha5}, optimal x: {x_opt:.6f}, f(x): {f(x_opt):.6f}")

