import autograd as auto
import autograd.numpy as np
import matplotlib.pyplot as plt
import scipy.optimize as opt

n = 10

x0 = np.full(10,-1)

def ex_rosenbrock(x,a):
    sum = 0

    n = x.shape[0]

    i = 1
    while i < n/2:
        first = x[2 * i] - (x[2 * (i - 1)])**2
        second = 1 - x[2 * (i - 1)]

        sum += a * first**2 + second**2
        i += 1
    return sum

def rosen_hess(x):
    x = np.asarray(x)
    H = np.diag(-400*x[:-1],1) - np.diag(400*x[:-1],-1)
    diagonal = np.zeros_like(x)
    diagonal[0] = 1200*x[0]**2-400*x[1]+2
    diagonal[-1] = 200
    diagonal[1:-1] = 202 + 1200*x[1:-1]**2 - 400*x[2:]
    H = H + np.diag(diagonal)
    return H

rosen = lambda x: ex_rosenbrock(x, 50)
rosen_der = auto.grad(rosen)
rosen_hess_auto = auto.jacobian(rosen_der)

res = opt.minimize(rosen, x0, method='Newton-CG',
               jac=rosen_der, hess=rosen_hess_auto,
               options={'gtol': 1e-12, 'disp': True, 'maxiter': 10000})
res.x

if not res.success:
    print("Hit max iterations")

print(res.message)   # explains why it stopped

print(res.jac)       # gradient at final point — should be ~0 at true x*

print("")
print("Best solution")
print(res.x)