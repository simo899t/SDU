import numpy as np
from autograd import grad
import time

n = 1000  # samples
d = 10    # features



# closed form ridge
def closed_ridge(X,y,lam):
    """
    Returns $w = (X^T X+ lambda I)^(-1) X^T y$
    as well as the time elapsed
    """
    t0 = time.perf_counter()
    A = X.T @ X + lam * np.eye(X.shape[1])
    B = X.T @ y

    result = np.linalg.solve(A,B)

    elapsed = time.perf_counter() - t0 
    return result, elapsed

def loss(w, xi, yi, lam):
    pred = xi @ w
    residual = pred - yi
    val = residual**2 + lam * (w @ w)
    return val

grad_loss = grad(loss)  # gradient w.r.t. first argument (w)


def gradient_descent(X, y, lam, alpha_step=0.001, max_iter=1000): # <- 1st order
    t0 = time.perf_counter()
    w = np.zeros(X.shape[1])
    i = 0
    while i < max_iter:
        idx = i % len(y)
        g = grad_loss(w, X[idx], y[idx], lam)
        step = alpha_step / (X[idx] @ X[idx] + lam)  # safe per-sample step
        w = w - step * g
        i += 1
    elapsed = time.perf_counter() - t0
    return w, elapsed


for n, d in [(1000, 10), (1000, 100), (1000, 500), (10000, 100)]:
    rng = np.random.default_rng(42)
    X_features = rng.standard_normal((n, d))          # n×d random features
    X = np.hstack([np.ones((n, 1)), X_features])      # n×(d+1), prepend bias column
    y = rng.standard_normal(n)
    w_closed, t_closed = closed_ridge(X,y,lam=0.1)
    w_gd, t_gd = gradient_descent(X,y,lam=0.1)

    print(f"n={n}, d={d}")
    #print(f"  Closed form:   {w_closed}, {t_closed:.4f}s")
    #print(f"  Gradient desc: {w_gd},     {t_gd:.4f}s")
    print(f"  w diff:       {np.linalg.norm(w_closed - w_gd):.4f}")