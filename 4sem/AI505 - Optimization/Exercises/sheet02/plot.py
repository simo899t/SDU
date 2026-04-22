import matplotlib.pyplot as plt
import numpy as np

def f(x1, x2):
    return x1**4 - 5*x1**2 + x2**2

# Starting point and search direction — adjust as needed
x0 = np.array([0.0, 1.0])
d  = np.array([1.0, -1.0])

alpha = np.linspace(-4, 4, 400)
phi = f(x0[0] + alpha * d[0], x0[1] + alpha * d[1])

plt.style.use('seaborn-v0_8-whitegrid')


plt.plot(alpha, phi, color='#4C72B0', linewidth=2.5)
plt.axhline(0, color='black', linewidth=0.8, linestyle='--', alpha=0.4)
plt.axvline(0, color='black', linewidth=0.8, linestyle='--', alpha=0.4)

plt.set_title(r'$\phi(\alpha) = f(\mathbf{x}_0 + \alpha \mathbf{d})$', fontsize=15, pad=12)
plt.set_xlabel(r'$\alpha$', fontsize=13)
plt.set_ylabel(r'$\phi(\alpha)$', fontsize=13)
plt.tick_params(labelsize=11)
plt.show()

