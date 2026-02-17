import numpy as np
import matplotlib.pyplot as plt
import time
import sympy as sp
import jax.numpy as jnp
from jax import grad
import numpy as np

# set a seed for reproducibility
np.random.seed(42)

num_of_points = 100

# generate x values
x = np.linspace(0, 10, num_of_points)

# to generate corresponding y values with some noise
# so lets define the function values
slope = 2
intercept = 5
y = slope * x + intercept + np.random.normal(0, 1, size=x.shape)

# Plot this shit with matplotlib
def plot_data(x, y, slope, intercept, calc_slope=None, calc_intercept=None):
    plt.figure(figsize=(12, 8))
    plt.scatter(x, y, alpha=0.7, color='blue', s=20, label='Noisy data points')
    plt.plot(x, slope * x + intercept, 'r--', linewidth=2, label=f'True line: y = {slope}x + {intercept}')
    
    # Add calculated line if provided
    if calc_slope is not None and calc_intercept is not None:
        plt.plot(x, calc_slope * x + calc_intercept, 'g-', linewidth=2, 
                label=f'Calculated line: y = {calc_slope:.4f}x + {calc_intercept:.4f}')
    
    plt.xlabel('x values')
    plt.ylabel('y values')
    plt.title('Linear Function with Random Noise - True vs Calculated Lines')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.show()


def plot_data2(x, y, slope, intercept, calc_slope, calc_intercept):
    """
    Plot data with both true line and calculated least squares line
    """
    plt.figure(figsize=(12, 8))
    plt.scatter(x, y, alpha=0.7, color='blue', s=20, label='Noisy data points')
    plt.plot(x, slope * x + intercept, 'r--', linewidth=2, label=f'True line: y = {slope}x + {intercept}')
    plt.plot(x, calc_slope * x + calc_intercept, 'g-', linewidth=2, 
            label=f'Calculated line: y = {calc_slope:.4f}x + {calc_intercept:.4f}')
    
    plt.xlabel('x values')
    plt.ylabel('y values')
    plt.title('Linear Function with Random Noise - True vs Calculated Lines')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.show()


# Calculate slope using the formula: slope = (N*sum(xy) - sum(x)*sum(y)) / (N*sum(x²) - (sum(x))²)
N = num_of_points
sum_x = np.sum(x)
sum_y = np.sum(y)
sum_xy = np.sum(x * y)
sum_x_squared = np.sum(x**2)

# Numerator: N*sum(xy) - sum(x)*sum(y)
numerator = N * sum_xy - sum_x * sum_y

# Denominator: N*sum(x²) - (sum(x))²
denominator = N * sum_x_squared - (sum_x)**2

# Calculate slope
calculated_slope = numerator / denominator

# print(f"Calculated slope: {calculated_slope:.4f}")

calculated_intercept = (sum_y - calculated_slope * sum_x) / N

# print(f"Calculated intercept: {calculated_intercept:.4f}")

print()

# Calculate eigenvalues of Hessian matrix H = [[2n, 2*sum_x], [2*sum_x, 2*sum_x_squared]]
# Quadratic formula: λ = (-b ± √(b² - 4ac)) / 2a where aλ² + bλ + c = 0
coeff_a = 1.0
coeff_b = -(2.0*N + 2.0*sum_x_squared)
coeff_c = 4.0*N*sum_x_squared - 4.0*(sum_x**2)

discriminant = coeff_b**2 - 4.0*coeff_a*coeff_c
lambda1 = (-coeff_b - np.sqrt(discriminant)) / (2.0*coeff_a)
lambda2 = (-coeff_b + np.sqrt(discriminant)) / (2.0*coeff_a)

print(f"Eigenvalue 1: {lambda1}")
print(f"Eigenvalue 2: {lambda2}")
print(f"Both positive: {lambda1 > 0 and lambda2 > 0}")

def y_predicted(x, b, m):
    """
    Calculate predicted y value for given x, intercept b, and slope m.
    """
    # print("y_preducted: " + str(b+m*x))
    return b + m * x

def squared_distance(y, y_pred):
    """
    Compute squared distance between actual and predicted values.
    """
    # print("squared_distance: " + str(y - y_pred))**2
    return (y - y_pred) ** 2

def get_predicted_values(x, b, m):
    """
    Returns a list of predicted y values for all x points.
    """
    predicted_values = []
    for xi in x:
        y_pred = y_predicted(xi, b, m)
        predicted_values.append(y_pred)
    # print("get_predicted_values: " + str(predicted_values))
    return predicted_values     

def sum_of_squares(x, y, b, m) -> float:
    """
    Sums the square of distances between actual and predicted values.
    Formula: f(b, m) = sum((y_i - (b + m * x_i))^2)
    """
    predicted_values = get_predicted_values(x, b, m)
    total_sum = 0
    for i in range(len(y)):
        squared_diff = squared_distance(y[i], predicted_values[i])
        total_sum += squared_diff
    # print("sum_of_sqares: " + str(total_sum))
    return total_sum

def partial_derivative_b(x, y, b, m) -> float:
    """
    Compute partial derivative with respect to b.
    Formula: ∂f/∂b = -2 * Σ(y_i - (b + m*x_i))
    """
    sum_dfdb = 0
    for i in range(len(y)):
        sum_dfdb += y[i] - y_predicted(x[i], b, m)
        # print(dfdb)
    dfdb = -2 * sum_dfdb
    # print("partial_derivative_b: " + str(dfdb))
    return dfdb

def partial_derivative_m(x, y, b, m) -> float:
    """
    Compute partial derivative with respect to m.
    Formula: ∂f/∂m = -2 * Σ((y_i - (b + m*x_i)) * x_i)
    """
    sum_dfdm = 0
    for i in range(len(y)):
        sum_dfdm += (y[i] - y_predicted(x[i], b, m)) * x[i]
        # print((y[i] - y_predicted(x[i], b, m)) * x[i])
        # print("sum: "+ str(sum_dfdm))
    dfdm = -2 * sum_dfdm
    # print("partial_derivative_m: " + str(dfdm))
    return dfdm

manual_db = partial_derivative_b(x, y, intercept, slope)
manual_dm = partial_derivative_m(x, y, intercept, slope)
# print(intercept, calculated_intercept)
# print(slope, calculated_slope)
# print(manual_dm)
# print(manual_db)


# Define symbolic variables
b_sym, m_sym = sp.symbols('b m')
x_sym, y_sym = sp.symbols('x y')

# Define cost function symbolically
cost_function = (y_sym - (b_sym + m_sym * x_sym))**2

# Compute symbolic derivatives
df_db_sym = sp.diff(cost_function, b_sym)
df_dm_sym = sp.diff(cost_function, m_sym)

def symbolic_derivatives(x, y, b, m):
    """Compute derivatives using symbolic differentiation"""
    df_db_total = 0
    df_dm_total = 0
    
    for i in range(len(x)):
        # Substitute values into symbolic expressions
        df_db_val = df_db_sym.subs([(x_sym, x[i]), (y_sym, y[i]), 
                                   (b_sym, b), (m_sym, m)])
        df_dm_val = df_dm_sym.subs([(x_sym, x[i]), (y_sym, y[i]), 
                                   (b_sym, b), (m_sym, m)])
        df_db_total += df_db_val
        df_dm_total += df_dm_val
    
    return float(df_db_total), float(df_dm_total)

# print("Symbolic derivatives:")
# print(f"∂f/∂b = {df_db_sym}")
# print(f"∂f/∂m = {df_dm_sym}")

# Use calculated values instead of arbitrary test values
sym_db, sym_dm = symbolic_derivatives(x, y, intercept, slope)




def cost_function_jax(params, x, y):
    """Cost function for automatic differentiation"""
    b, m = params
    predictions = b + m * x
    return jnp.sum((y - predictions)**2)

# Create gradient function automatically
grad_function = grad(cost_function_jax)

def automatic_derivatives(x, y, b, m):
    """Compute derivatives using automatic differentiation"""
    params = jnp.array([b, m], dtype=jnp.float32)
    x_jax = jnp.array(x, dtype=jnp.float32)
    y_jax = jnp.array(y, dtype=jnp.float32)
    
    gradients = grad_function(params, x_jax, y_jax)
    return float(gradients[0]), float(gradients[1])

auto_db, auto_dm = automatic_derivatives(x,y,intercept, slope)

print(f"\nUsing true values b={intercept:.4f}, m={slope:.4f}:")
print(f"Manual:   ∂f/∂b = {manual_db:.6f}, ∂f/∂m = {manual_dm:.6f}")
print(f"Symbolic: ∂f/∂b = {sym_db:.6f}, ∂f/∂m = {sym_dm:.6f}")
print(f"Automatic:∂f/∂b = {auto_db:.6f}, ∂f/∂m = {auto_dm:.6f}")

print("============")



# Manual implementation
start_time = time.time()
manual_db = partial_derivative_b(x, y, intercept, slope)
manual_dm = partial_derivative_m(x, y, intercept, slope)
manual_time = time.time() - start_time

# Symbolic differentiation
start_time = time.time()
sym_db, sym_dm = symbolic_derivatives(x, y, intercept, slope)
symbolic_time = time.time() - start_time

# Automatic differentiation
start_time = time.time()
auto_db, auto_dm = automatic_derivatives(x, y, intercept, slope)
auto_time = time.time() - start_time






print("Comparison Results:")
print(f"Manual:     ∂f/∂b = {manual_db:.6f}, ∂f/∂m = {manual_dm:.6f}")
print(f"Symbolic:   ∂f/∂b = {sym_db:.6f}, ∂f/∂m = {sym_dm:.6f}")
print(f"Automatic:  ∂f/∂b = {auto_db:.6f}, ∂f/∂m = {auto_dm:.6f}")

print(f"\nTiming Comparison:")
print(f"Manual: {manual_time:.6f} seconds")
print(f"Symbolic: {symbolic_time:.6f} seconds") 
print(f"Automatic: {auto_time:.6f} seconds")

# Only run calculations when this script is executed directly
if __name__ == "__main__":
    # Get predicted values using calculated slope and intercept
    #predicted_values = get_predicted_values(x, calculated_intercept, calculated_slope)
    
    print(f"True slope: {slope:.4f}")
    print(f"True intercept: {intercept:.4f}")
    print(f"Calculated slope: {calculated_slope:.4f}")
    print(f"Calculated intercept: {calculated_intercept:.4f}")
    #print(f"First 5 predicted values: {predicted_values[:5]}")
    #print(f"First 5 actual values: {y[:5]}")
    #
    # Calculate sum of squares
    #sos = sum_of_squares(x, y, calculated_intercept, calculated_slope)
    #print(f"Sum of squares: {sos:.4f}")
    
    # Plot with both true and calculated lines
    # plot_data2(x, y, slope, intercept, calculated_slope, calculated_intercept)
    
    print("Script completed successfully - plotting enabled")

def solve_least_squares(x, y):
    """
    Solve for b and m using the specific linear system:
    n*b + (Σx_i)*m = Σy_i
    (Σx_i)*b + (Σx_i²)*m = Σx_i*y_i
    Returns: (intercept, slope)
    """
    n = len(x)
    sum_x = np.sum(x)
    sum_y = np.sum(y)
    sum_xy = np.sum(x * y)
    sum_x_squared = np.sum(x**2)
    
    # Set up the coefficient matrix A and right-hand side vector b
    # A * [b, m]^T = rhs
    A = np.array([[n, sum_x],
                  [sum_x, sum_x_squared]])
    
    rhs = np.array([sum_y, sum_xy])
    
    # Solve the linear system A * params = rhs
    params = np.linalg.solve(A, rhs)
    
    # Return intercept (b) and slope (m)
    return params[0], params[1]

def solve_least_squares_formula(x, y):
    """
    Calculate b and m directly from explicit formulas.
    Returns: (intercept, slope)
    """
    N = len(x)
    sum_x = np.sum(x)
    sum_y = np.sum(y)
    sum_xy = np.sum(x * y)
    sum_x_squared = np.sum(x**2)
    
    denominator = N * sum_x_squared - sum_x**2
    b = (sum_x_squared * sum_y - sum_x * sum_xy) / denominator
    m = (N * sum_xy - sum_x * sum_y) / denominator
    return b, m

# Method 1: QR Decomposition
qr_b, qr_m = solve_least_squares(x, y)
print(f"QR Decomposition Method:")
print(f"  Intercept (b): {qr_b:.8f}")
print(f"  Slope (m):     {qr_m:.8f}")

# Method 2: Explicit Formula
formula_b, formula_m = solve_least_squares_formula(x, y)
print(f"\nExplicit Formula Method:")
print(f"  Intercept (b): {formula_b:.8f}")
print(f"  Slope (m):     {formula_m:.8f}")

# Calculate differences
qr_formula_diff_b = abs(qr_b - formula_b)
qr_formula_diff_m = abs(qr_m - formula_m)

print(f"\nDifferences:")
print(f"  QR vs Formula - Intercept: {qr_formula_diff_b:.2e}")
print(f"  QR vs Formula - Slope:     {qr_formula_diff_m:.2e}")




iterations = 1000

  # Time QR method
start = time.time()
for _ in range(iterations):
    solve_least_squares(x, y)
qr_time = (time.time() - start)/iterations
# Time formula method
start = time.time()
for _ in range(iterations):
    solve_least_squares_formula(x, y)
formula_time = (time.time() - start)/iterations

print(f"\nTiming (avg iteration):")
print(f"  QR Decomposition: {qr_time:.6f} seconds")
print(f"  Explicit Formula: {formula_time:.6f} seconds")
print(f"  Speed ratio: {qr_time/formula_time:.2f}x")

# Verify accuracy with true values
true_b, true_m = intercept, slope
print(f"\nAccuracy compared to true values:")
print(f"  True values: b={true_b}, m={true_m}")
print(f"  QR Error - b: {abs(qr_b - true_b):.6f}, m: {abs(qr_m - true_m):.6f}")
print(f"  Formula Error - b: {abs(formula_b - true_b):.6f}, m: {abs(formula_m - true_m):.6f}")

# # Example data
# x = np.array([1, 2, 3, 4, 5])
# y = np.array([2, 4, 5, 4, 5])
# 
# # Define the loss function
# def f(b, m):
#     return np.sum((y - (b + m * x))**2)
# 
# # Create a grid of b and m values
# b_vals = np.linspace(-2, 6, 100)
# m_vals = np.linspace(-1, 3, 100)
# B, M = np.meshgrid(b_vals, m_vals)
# 
# # Compute f(b, m) over the grid
# Z = np.array([[f(b, m) for b in b_vals] for m in m_vals])
# 
# # --- Surface plot ---
# fig = plt.figure(figsize=(10, 6))
# ax = fig.add_subplot(111, projection='3d')
# ax.plot_surface(B, M, Z, cmap='viridis', alpha=0.9)
# ax.set_xlabel('b')
# ax.set_ylabel('m')
# ax.set_zlabel('f(b, m)')
# ax.set_title('Sum of Squared Errors Surface')
# plt.show()
# 
# # --- Contour plot ---
# plt.figure(figsize=(8, 6))
# plt.contour(B, M, Z, levels=30, cmap='viridis')
# plt.xlabel('b')
# plt.ylabel('m')
# plt.title('f(b, m) Contour Plot')
# plt.colorbar(label='f(b, m)')
# plt.show()

def gradient_descent(x, y, b, m, learning_rate=0.01, max_iterations=1000):
    """
    Minimize f(b, m) using gradient descent.
    """
    epsilon = 1e-6
    
    i = 0
    while i < max_iterations:
        dfdb = partial_derivative_b(x, y, b, m)
        dfdm = partial_derivative_m(x, y, b, m)
        
        # Check for numerical instability (too large numbers or division by 0)
        if not (np.isfinite(dfdb) and np.isfinite(dfdm)):
            return b, m
        
        if abs(dfdb) < epsilon and abs(dfdm) < epsilon:
            return b, m
        
        # Update parameters for next iteration
        b = b - learning_rate * dfdb
        m = m - learning_rate * dfdm
        i += 1
    # max iterations have been reached
    return b, m

# keep low
learning_rate = 0.0001

ls_b, ls_m = solve_least_squares(x, y)
formula_b, formula_m = solve_least_squares_formula(x, y)
gd_b, gd_m = gradient_descent(x, y, 0, 0, learning_rate=learning_rate, max_iterations=10000)

print("\nRegression Results:")
print(f"Least Squares:     b = {ls_b:.6f}, m = {ls_m:.6f}")
print(f"Explicit Formula:     b = {formula_b:.6f}, m = {formula_m:.6f}")
print(f"Gradient Descent:     b = {gd_b:.6f}, m = {gd_m:.6f}")

print("\nDifferences:")
print(f"LS vs Formula:        Δb = {abs(ls_b-formula_b):.2e}, Δm = {abs(ls_m-formula_m):.2e}")
print(f"LS vs GD:             Δb = {abs(ls_b-gd_b):.2e}, Δm = {abs(ls_m-gd_m):.2e}")
print(f"Formula vs GD:        Δb = {abs(formula_b-gd_b):.2e}, Δm = {abs(formula_m-gd_m):.2e}")
