import matplotlib.pyplot as plt
import numpy as np

def f(x):
    return 8 * x[0] + 12 * x[1] + x[0]**2 - 2 * x[1]**2


x0 = np.linspace(-10, 10, 200)
x1 = np.linspace(-10, 10, 200)
X0, X1 = np.meshgrid(x0, x1)
Z = f([X0, X1])

fig = plt.figure(figsize=(12, 5))

ax1 = fig.add_subplot(121, projection="3d")
ax1.plot_surface(X0, X1, Z, cmap="viridis", alpha=0.8)
ax1.set_xlabel("x0")
ax1.set_ylabel("x1")
ax1.set_zlabel("f(x)")
ax1.set_title("f(x) = 8x₀ + 12x₁ + x₀² − 2x₁²")

ax2 = fig.add_subplot(122)
cp = ax2.contourf(X0, X1, Z, levels=30, cmap="viridis")
fig.colorbar(cp, ax=ax2)
ax2.set_xlabel("x0")
ax2.set_ylabel("x1")
ax2.set_title("Contour plot")

plt.tight_layout()
plt.show()
