import autograd as ag
import autograd.numpy as anp
import matplotlib.pyplot as plt
import numpy as np

def f(x):
    return x[0]**4 - 5*x[0]**2 + x[1]**2

nabla_f = ag.grad(f)

x0 = anp.array([0.0, 1.0])
d  = anp.array([1.0, -1.0])

def strong_backtracking(f, nabla, x, d, alpha=1, beta=1e-4, sigma=0.1):
    y0, g0, y_prev, alpha_prev = f(x), nabla(x) @ d, None, 0
    alpha_lo, alpha_hi = None, None
    rejected     = []   # failed, shrink
    passed  = []   # passed, continue

    # bracket phase
    while True:
        y = f(x + alpha*d)
        if y > y0 + beta*alpha*g0 or (y_prev is not None and y >= y_prev):
            rejected.append(alpha)
            alpha_lo, alpha_hi = alpha_prev, alpha
            print(f"low: {alpha_lo:.6f}  high:{alpha_hi:.6f}")
            break
        dir_gradient = nabla(x + alpha*d) @ d
        if abs(dir_gradient) <= -sigma * g0:
            print("strong Wolfe in " r"$\alpha$" f"{alpha:.6f}")
            return alpha, rejected, passed
        elif dir_gradient >= 0:
            rejected.append(alpha)
            alpha_lo, alpha_hi = alpha, alpha_prev
            print(f"bracket done: low={alpha_lo:.6f}  high={alpha_hi:.6f}")
            break
        else:
            passed.append(alpha)   # sufficient decrease holds, keep expanding
        y_prev, alpha_prev, alpha = y, alpha, 2 * alpha

    # zoom phase
    ylo = f(x + alpha_lo*d)
    while abs(alpha_hi - alpha_lo) > 1e-10:
        alpha = (alpha_lo + alpha_hi)/2
        y = f(x + alpha*d)
        print(f"alpha={alpha:.6f}  f={y:.6f}  low={alpha_lo:.6f}  high={alpha_hi:.6f}")
        if y > y0 + beta*alpha*g0 or y >= ylo:
            rejected.append(alpha)
            alpha_hi = alpha
        else:
            g = nabla(x + alpha*d) @ d
            if abs(g) <= -sigma*g0:
                print(f"Zoom done: lo={alpha_lo:.6f}  hi={alpha_hi:.6f}")
                return alpha, rejected, passed
            elif g*(alpha_hi - alpha_lo) >= 0:
                rejected.append(alpha)
                alpha_hi = alpha_lo
            else:
                passed.append(alpha)  # updates alpha_lo, still zooming
            alpha_lo = alpha
    print(f"Zoom done: lo={alpha_lo:.6f}  hi={alpha_hi:.6f}")
    return alpha_lo, rejected, passed

accepted_alpha, rejected_alphas, provisional_alphas = strong_backtracking(f, nabla_f, x0, d)

print(f"Accepted alpha: {accepted_alpha}")

# --- Plot ---
def phi_scalar(a):
    x = x0 + a * d
    return float(f(x))

alpha_set = np.linspace(-4, 4, 400)
phi_vals  = np.array([phi_scalar(a) for a in alpha_set])

plt.style.use('seaborn-v0_8-whitegrid')
fig, ax = plt.subplots(figsize=(9, 5))

ax.plot(alpha_set, phi_vals, color='#4C72B0', linewidth=2.5, label=r'$\phi(\alpha)$')
ax.axhline(0, color='black', linewidth=0.8, linestyle='--', alpha=0.4)
ax.axvline(0, color='black', linewidth=0.8, linestyle='--', alpha=0.4)

# provisional: passed sufficient decrease, used to advance bracket
if provisional_alphas:
    prov = np.array(provisional_alphas)
    ax.scatter(prov, [phi_scalar(a) for a in prov],
               color='gold', zorder=5, s=70, label='Passed (bracket step)')

# rejected: failed sufficient decrease or curvature
if rejected_alphas:
    rej = np.array(rejected_alphas)
    ax.scatter(rej, [phi_scalar(a) for a in rej],
               color='tomato', zorder=5, s=70, marker='x', linewidths=2,
               label='Rejected')

# final accepted alpha
ax.scatter([accepted_alpha], [phi_scalar(accepted_alpha)],
           color='green', zorder=6, s=150, marker='*',
           label=f'Final $\\alpha^* = {accepted_alpha:.4f}$')

ax.set_title(r'Strong Backtracking  $\alpha^* = $' + f'{accepted_alpha:.4f}', fontsize=15, pad=12)
ax.set_xlabel(r'$\alpha$', fontsize=13)
ax.set_ylabel(r'$\phi(\alpha)$', fontsize=13)
ax.tick_params(labelsize=11)
ax.legend(fontsize=11)
plt.tight_layout()
plt.show()
