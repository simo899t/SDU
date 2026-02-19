import numpy as np
import matplotlib.pyplot as plt

# Create grid of x and y values
x = np.linspace(-3, 3, 200)
y = np.linspace(-3, 3, 200)
X, Y = np.meshgrid(x, y)

# Define a function to contour
Z = 8*X + 12*Y + X**2 - 2*Y**2

# Create contour plot
plt.figure(figsize=(6, 5))
contours = plt.contour(X, Y, Z, levels=15, cmap='viridis')
plt.clabel(contours, inline=True, fontsize=8)
plt.title("Contour Plot Example")
plt.xlabel("X")
plt.ylabel("Y")
plt.show()