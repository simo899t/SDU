#set document(title: "CalcProj", author: "Simon Holm")
#set page(
  paper: "us-letter",
  margin: (left: 3cm, right: 3cm, top: 2cm, bottom: 2cm)
)
#set text(font: "New Computer Modern", size: 11pt)
#set math.equation(numbering: "(1)")
#set heading(numbering: "1.")
#set page(
  numbering: "1 / 1",
)

// Set matrix delimiters to square brackets globally
#set math.mat(delim: "[")

#let evaluated(expr, size: 100%) = $lr(#expr|, size: #size)$

// Custom box function for easy use
#let box(content) = block(
  fill: rgb("#f0f0f0"),
  inset: 10pt,
  radius: 4pt,
  content
)

// Title page
#align(center)[
  #text(size: 18pt, weight: "bold")[Calculus Project]
  
  #v(1em)
  
  Simon Holm \
    AI503: Calculus \
    Teacher: Shan Shan
]

#pagebreak()

// Table of contents - only show level 1 headings (=)
#outline(depth: 1, indent: auto)

#pagebreak()

= Task 1: Create Example Data

== Question:
  
To create a synthetic dataset, let's generate x-values and corresponding y-values with some added noise. Use the code below to create a set of points that should roughly lie on a line. You may choose different values for the true slope and intercept.
  
#box[
  ```py
  import numpy as np
  import random
  
  # Set a seed for reproducibility
  np.random.seed(42)
  
  # Generate x-values
  x = np.linspace(0, 10, 50)
  # Define the true slope and intercept
  true_m = 2
  true_b = 5
  # Generate y-values with some noise
  y = true_b + true_m * x + np.random.normal(0, 1, len(x))
  ```
  ]
  Write Python code to plot the example data and plot the line given by $y = "true_b" + "true_m" times x$.
  
== Answer:

The following code below creates a set of points $(x,y)$ which roughly lie on a line.
#box[
  ```py
  import numpy as np
  import random

  # set a seed for reproducibility
  np.random.seed(42)

  # define the number of points in the dataset
  number_of_points = 100

  # generate x values
  x = np.linspace(0, 10, number_of_points)

  # to generate corresponding y values with some noise
  # so lets define the function values
  slope = 2
  intercept = 5
  y = slope * x + intercept + np.random.normal(0, 1, size=x.shape)
  ```
]
#pagebreak()
The data has then been plotted using python libary "matplotlib"

#box[
  ```py
import matplotlib.pyplot as plt

# *data implementation*

# Ploting with matplotlib
plt.figure(figsize=(10, 6))
plt.scatter(x, y, alpha=0.7, color='blue', s=20, label='Noisy data points')
plt.plot(x, slope * x + intercept, 'r--', linewidth=2, label=f'True line: y = {slope}x + {intercept}')
plt.xlabel('x values')
plt.ylabel('y values')
plt.title('Linear Function with Random Noise')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()
  ```
]

#figure(
  caption: [Noisy linear data generated with slope 2 and intercept 5; dashed red line shows the true model $y = 2x+5$.]
)[
  #image("images/dataPlot_Task1.png")
] <fig-task1-data>


#pagebreak()
= Task 2: Modeling the Least Squares Line

== Question (a):
  
For each data point $(x_i, y_i)$, show that the corresponding point on the least squares line has a y-coordinate of $b + m x_i$.

== Answer (a):

The objective is to show that using the least squares line, it is possible to describe each point as a corresponding cordinate with a line $y=m x+b$

First calculate how much $y$ changes per change in $x$

This is done by 
$ "slope" = (N sum(x y)-sum x sum y)/N(sum x^2-(sum x)^2) $
Where N is the number of datapoints.
This can easily be done using python libary numpy
#box[
  ```py
N = numOfPoints
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

# Then 
print(f"Calculated slope: {calculated_slope:.4f}")

[OUTPUT] Calculated slope: 2.0138
  ```
]

The calculated slope $approx 2.0138$. And since the true slope of the datapoints are based on a $"slope" = 2$ the small difference of $0.0138$ makes good sense, since the datapoints has been scattered from the true line of $y = 2x + b$.

#pagebreak()

Now to find the intercept, this is done by
$ "intercept" = (sum_y - "calculated_slope" * sum_x) / N $
Where N is still the number of datapoints.

This is calculated using python libary numpy
#box[
  ```py
  # slope calculation from previous code block

  calculated_intercept = (sum_y - calculated_slope * sum_x) / N

  print(f"Calculated intercept: {calculated_intercept:.4f}")

  [OUTPUT] Calculated intercept: 4.8272
  ```
]

So by this the calculated intercept is calculated to be $approx 4.8272$.

Finally by this the least squares line can be described as
$  y = 2.0138 x + 4.8272$

#image("images/dataPlot_Task3.png")

The graph above clearly shows that the calculated line aligns roughly with both the data as well as the actual true line $y = 2x+5$.

#pagebreak()
== Question (b):

Implement a Python function, y_predicted(x, b, m), that calculates $b + m x$ for a given $x$, $b$, and $m$.

== Answer (b):
For this task the following python function has been implemented:
#box[```py
def y_predicted(x, b, m):
    """
    Calculate predicted y value for given x, intercept b, and slope m.
    """
    return b + m * x
    
```]

#pagebreak()
= Task 3: Calculating Squared Distances


== Question (a):
  
Show that for each data point $(x_i, y_i)$, the square of the vertical distance from it to the point on the line is $(y_i - (b + m x_i))^2$.

== Answer (a):

The vertical distance from the point $(x_i, y_i)$ to the line $y = b + m x$ is given by the difference in their y-coordinates:

$ "Distance" = y_i - (b + m x_i) $

To find the square of this distance, we simply square the expression:

$ "Squared Distance" = (y_i - (b + m x_i))^2 $

This shows that for each data point $(x_i, y_i)$, the square of the vertical distance from it to the point on the line is indeed $(y_i - (b + m x_i))^2$.

#v(1em)
== Question (b):
  
Write a Python function, squared_distance(y, y_pred), to compute $(y - y_"predicted")^2$ for values $y$ and $y_"predicted"$.



== Answer (b):
For this task the following python function has been implemented:
#box[```python
def squared_distance(y, y_pred):
    """
    Compute squared distance between actual and predicted values.
    """
    return (y - y_pred) ** 2
```]

#pagebreak()
= Task 4: Defining and Minimizing the Cost Function

== Question (a):
  
Define the cost function $f(b, m)$ as the sum of all $n$ squared distances:
  
$ f(b, m) = sum_(i=1)^n (y_i - (b + m x_i))^2 $
  
Show that the partial derivatives $(partial f) / (partial b)$ and $(partial f) / (partial m)$ are:
  
$ (partial f) / (partial b) = -2 sum_(i=1)^n (y_i - (b + m x_i)) $
  
$ (partial f) / (partial m) = -2 sum_(i=1)^n (y_i - (b + m x_i)) dot.c x_i $

== Answer (a):

Partial differentiation for both $b$ and $m$ with use of the chainrule $ (partial)/(partial x)f(g(x)) = f'(g(x))dot g(x) $
This is done firstly with respect to $b$
$ (partial f) / (partial b) = sum_(i=1)^n (partial)/(partial x)(y_i - (b + m x_i))^2dot (partial)/(partial b) y_i - (b + m x_i) $

$ (partial f) / (partial b) = sum_(i=1)^n 2(y_i - (b + m x_i)) dot (-1) $
$ (partial f) / (partial b) = -2sum_(i=1)^n (y_i - (b + m x_i)) $

now for the derivative with respect to $m$  
$ (partial f) / (partial m) = sum_(i=1)^n (partial)/(partial m)(y_i - (b + m x_i))^2dot (partial)/(partial m) y_i - (b + m x_i) $

$ (partial f) / (partial m) = sum_(i=1)^n 2(y_i - (b + m x_i)) dot (-x_i) $


$ (partial f) / (partial m) = -2 sum_(i=1)^n (y_i - (b + m x_i)) dot x_i $

#pagebreak()
== Question (b):
  
Write three Python functions:

#enum(
  numbering: "(1)",
  [sum_of_squared_distances(x, y, b, m)],
  [partial_derivative_b], 
  [partial_derivative_m]
)
  
to compute $f(b, m)$, $(partial f) / (partial b)$, and $(partial f) / (partial m)$.


== Answer (b):
For this task the function get_predicted_values have implemented
#box[
  ```py
  def get_predicted_values(x, b, m):
    """
    Returns a list of predicted y values for all x points.
    """
    predicted_values = []
    for xi in x:
        y_pred = y_predicted(xi, b, m)
        predicted_values.append(y_pred)
    return predicted_values
  ```
]
The function implemented above calculates all the predicted values for the dataset.
=== sum_of_squares()

#box[```python
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
    return total_sum

```]
The implementation above, uses a help function (get_predicted_values) to firstly get all predicted values for the x-values in the dataset. Then it computes the sum of squares using the get_predicted_values function.

#pagebreak()

=== partial_derivative_b
#box[```py
def partial_derivative_b(x, y, b, m):
    """
    Compute partial derivative with respect to b.
    Formula: ∂f/∂b = -2 * sum(y_i - (b + m*x_i))
    """
    dfdb = 0
    for i in range(len(y)):
        dfdb += y[i] - y_predicted(x[i], b, m)
    return -2 * dfdb
```]

=== partial_derivative_m
#box[```py
def partial_derivative_m(x, y, b, m):
    """
    Compute partial derivative with respect to m.
    Formula: ∂f/∂m = -2 * sum((y_i - (b + m*x_i)) * x_i)
    """
    dfdm = 0
    for i in range(len(y)):
        dfdm += (y[i] - y_predicted(x[i], b, m)) * x[i]
    return -2 * dfdm
```]

#pagebreak()

== Question (c):
  
Write Python code to compute $(partial f)/ (partial b)$ and $(partial f) / (partial m)$ using Symbolic differentiation and Automatic differentiation. Check your answers and compare the computation time.

== Answer (c):

=== Symbolic Differentiation with SymPy
#box[```python
import sympy as sp
import time

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
    
    return float(df_db_total), float(df_dm_sym)

```]
#pagebreak()
=== Automatic Differentiation with JAX
#box[```python
import jax.numpy as jnp
from jax import autograd

def cost_function_jax(params, x, y):
    """Cost function for automatic differentiation"""
    b, m = params
    predictions = b + m * x
    return jnp.sum((y - predictions)**2)

# Create gradient function automatically
grad_function = grad(cost_function_jax)

def automatic_derivatives(x, y, b, m):
    """Compute derivatives using automatic differentiation"""
    params = jnp.array([b, m])
    x_jax = jnp.array(x)
    y_jax = jnp.array(y)
    
    gradients = grad_function(params, x_jax, y_jax)
    return float(gradients[0]), float(gradients[1])
```]
#pagebreak()

=== Comparison and Timing
#box[```python
import time

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

[OUTPUT]
Comparison Results:
Manual:     ∂f/∂b = 20.769303, ∂f/∂m = 80.393319
Symbolic:   ∂f/∂b = 20.769303, ∂f/∂m = 80.393319
Automatic:  ∂f/∂b = 20.769289, ∂f/∂m = 80.393196

Timing Comparison:
Manual: 0.000046 seconds
Symbolic: 0.146954 seconds
Automatic: 0.002684 seconds
```]

*Results show all methods give nealy identical results, with automatic and maunal differentiation being fastest.*

#pagebreak()
= Task 5: Setting Up and Solving the System of Equations

== Question (a):
  
Show that setting $(partial f) / (partial b) = 0$ and $(partial f )/ (partial m) = 0$ results in the linear system:
  
$ n b + (sum_(i=1)^n x_i) m = sum_(i=1)^n y_i $
  
$ (sum_(i=1)^n x_i) b + (sum_(i=1)^n x_i^2) m = sum_(i=1)^n x_i y_i $
  
Explain why solving for $b$ and $m$ minimizes the cost function.

== Answer (a):

When setting $(partial f) / (partial b) = 0$ and $(partial f )/ (partial m) = 0$

this becomes

$ -2sum_(i=1)^n (y_i - (b + m x_i))=0 $
$ -2 sum_(i=1)^n (y_i - (b + m x_i)) dot x_i=0 $

Start by $(partial f) / (partial b)$

#align($
          -2 sum_(i=1)^n (y_i - (b + m x_i)) &= 0 \
          sum_(i=1)^n (y_i - b - m x_i) &= 0 \
          sum_(i=1)^n y_i - sum_(i=1)^n b - sum_(i=1)^n m x_i &= 0 \
          sum_(i=1)^n y_i - n b - m sum_(i=1)^n x_i &= 0 \
          n b + m sum_(i=1)^n x_i &= sum_(i=1)^n y_i
        $)

#pagebreak()
Then $(partial f) / (partial m)$

#align($
         -2 sum_(i=1)^n (y_i - (b + m x_i)) dot x_i &=0 \
         sum_(i=1)^n (y_i - (b + m x_i)) dot x_i &=0 \
         sum_(i=1)^n (y_i x_i - (b x_1 + m x_i^2)) &=0 \
         sum_(i=1)^n (y_i x_i) - (b sum_(i=1)^n x_1 + m sum_(i=1)^n x_i^2) &=0 \
         sum_(i=1)^n (y_i x_i) &= (b sum_(i=1)^n x_1 + m sum_(i=1)^n x_i^2) \
       $)

This gives us the system of linear equations:
$ n b + (sum_(i=1)^n x_i) m = sum_(i=1)^n y_i $
$ (sum_(i=1)^n x_i) b + (sum_(i=1)^n x_i^2) m = sum_(i=1)^n x_i y_i $

*Why does solving for $b$ and $m$ minimize the cost function?*

Setting the partial derivatives equal to zero finds the critical points of the cost function $f(b,m)$. Since our cost function is:
$ f(b, m) = sum_(i=1)^n (y_i - (b + m x_i))^2 $

To confirm this critical point is indeed a minimum, we examine the *Hessian matrix* containing all second-order partial derivatives:

$ H = mat(
  (partial^2 f)/(partial b^2), (partial^2 f)/(partial b partial m);
  (partial^2 f)/(partial m partial b), (partial^2 f)/(partial m^2)
) $

Computing the second derivatives:
$ (partial^2 f)/(partial b^2) = (partial)/(partial b)(-2sum_(i=1)^n (y_i - (b + m x_i))) = 2n $
$ (partial^2 f)/(partial b partial m) = (partial)/(partial m)(-2sum_(i=1)^n (y_i - (b + m x_i))) = 2sum_(i=1)^n x_i $  
$ (partial^2 f)/(partial m^2) = (partial)/(partial m)(-2 sum_(i=1)^n (y_i - (b + m x_i)) dot.c x_i) = 2sum_(i=1)^n x_i^2 $

#pagebreak()
This gives us:
$ H = mat(
  2n, 2sum_(i=1)^n x_i;
  2sum_(i=1)^n x_i, 2sum_(i=1)^n x_i^2
) $

Then find the eigen values

$ det(A-lambda I) = det(mat(
  2n-lambda, 2sum_(i=1)^n x_i;
  2sum_(i=1)^n x_i, 2sum_(i=1)^n x_i^2-lambda)) = 0 $

$ (2n-lambda) dot (2sum_(i=1)^n x_i^2-lambda)- (2sum_(i=1)^n x_i)^2 = 0 $

$ 4n hat(x^2)-2n lambda - 2hat(x^2)lambda + lambda^2 - 4hat(x)^2 =0 $
where $hat(x) = sum_(i=1)^n x_i $

$ lambda^2 + (-2n-2hat(x^2)) lambda + 4n hat(x^2)-4hat(x)^2 = 0 $

since $x = frac(-b plus.minus sqrt(b^2-4a c), 2a)$

so..
$ lambda = frac(-(-2n-2hat(x^2))-sqrt((-2n-2hat(x^2))^2-4(4n hat(x^2)-4hat(x)^2)), 2) $
and
$ lambda = frac(-(-2n-2hat(x^2))+sqrt((-2n-2hat(x^2))^2-4(4n hat(x^2)-4hat(x)^2)), 2) $

#box(
  ```py
  coeff_a = 1.0
  coeff_b = -(2.0*N + 2.0*sum_x_squared)
  coeff_c = 4.0*N*sum_x_squared - 4.0*(sum_x**2)

  discriminant = coeff_b**2 - 4.0*coeff_a*coeff_c
  lambda1 = (-coeff_b - np.sqrt(discriminant)) / (2.0*coeff_a)
  lambda2 = (-coeff_b + np.sqrt(discriminant)) / (2.0*coeff_a)

  print(f"Eigenvalue 1: {lambda1}")
  print(f"Eigenvalue 2: {lambda2}")
  print(f"Both positive: {lambda1 > 0 and lambda2 > 0}")
  
  [OUTPUT]: 
  Eigenvalue 1: 49.63981706705363
  Eigenvalue 2: 6850.696883269647
  Both positive: True
  ```
)
Since $lambda_1approx 49.6$ and $lambda_2 approx 6850.7$ both are positive the critical point must be a local minimum

#pagebreak()
== Question (b):
  
Write a Python function, solve_least_squares(x, y), to solve for $b$ and $m$ using this system of linear equations.

== Answer (b):
#box[```python
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
    # A * [b, m]^T = constants_vector
    A = np.array([[n, sum_x],
                  [sum_x, sum_x_squared]])
    
    constants_vector = np.array([sum_y, sum_xy])
    
    # Solve the linear system A * params = constants_vector
    result = np.linalg.solve(A, constants_vector)
    
    # Return intercept (b) and slope (m)
    return result[0], result[1]
```
]

#pagebreak()
= Task 6: Deriving Explicit Formulas for b and m

== Question (a):
  
Solve the equations above to derive explicit formulas for $b$ and $m$:
  
$ b = ((sum_(i=1)^n x_i^2)(sum_(i=1)^n y_i) - (sum_(i=1)^n x_i)(sum_(i=1)^n x_i y_i)) / (n sum_(i=1)^n x_i^2 - (sum_(i=1)^n x_i)^2) $
  
$ m = (n sum_(i=1)^n x_i y_i - (sum_(i=1)^n x_i)(sum_(i=1)^n y_i)) / (n sum_(i=1)^n x_i^2 - (sum_(i=1)^n x_i)^2) $

== Answer (a):

for simplicitys sake $sum_(i=1)^n x_i = sum x$

start with the system of equations:
$ n b + (sum_(i=1)^n x_i) m = sum_(i=1)^n y_i $
$ (sum_(i=1)^n x_i) b + (sum_(i=1)^n x_i^2) m = sum_(i=1)^n x_i y_i $

then isolate $b$ and $m$ from the  equations:
$ b = 1/n (sum y-m sum x) $
$ m = frac(sum x y- b sum x,sum x^2) $

now express $b$ with the equation for $m$
$ b = 1/n (sum y-(frac(sum x y- b sum x,sum x^2)) sum x) $

now simplify with common fraction rule $a/b-c/d = (a d-b c)/(b d)$

$ b = (sum y)/n-(frac(sum x y-  b sum x,n sum x^2)) sum x = frac(n sum y sum x^2-n sum x sum x y-n b(sum x)^2, n^2sum x^2)  $

now simplify further
$ b = (sum y sum x^2-sum x sum x y - b(sum x)^2)/(n sum x^2) $
#pagebreak()

This can be rewritten in order to isolate b:

#align($
  b &= (sum y sum x^2-sum x sum x y)/(n sum x^2) - (b(sum x)^2)/(n sum x^2) \
  b + (b(sum x)^2)/(n sum x^2) &= (sum y sum x^2-sum x sum x y)/(n sum x^2) \
$)

Multiply both sides by $n sum x^2$

#align($
  b dot n sum x^2 + b(sum x)^2 &= sum y sum x^2-sum x sum x y \
  b (n sum x^2 + (sum x)^2) &= sum y sum x^2-sum x sum x y \
  b &= (sum y sum x^2-sum x sum x y)/(n sum x^2 - (sum x)^2)
$)
Now to express $m$

First express $m$ using the equation from $b$
$ m=(sum x y-b sum x)/(sum x^2) = (sum x y-(1/n (sum y-m sum x)) sum x)/(sum x^2) $
Now simplify
#align(
  $ m&=(sum x y-(1/n (sum y-m sum x)) sum x)/(sum x^2)\ 
    &=(sum x y-(1/n sum y-1/n m sum x) sum x)/(sum x^2)\ 
    &=(sum x y-(1/n sum y sum x-1/n m (sum x)^2))/(sum x^2)\
    &=(sum x y-(sum y sum x)/n+(m (sum x)^2)/n)/(sum x^2) $
)
#pagebreak()
This can be rewritten in order to isolate b:
#align(
  $   m&=(sum x y-(sum y sum x)/n+(m (sum x)^2)/n)/(sum x^2)\
      m&=(sum x y)/(sum x^2)-(sum y sum x)/(n sum x^2)+(m (sum x)^2)/(n sum x^2)\
      m-(m (sum x)^2)/(n sum x^2)&= (sum x y)/(sum x^2)-(sum y sum x)/(n sum x^2)\
      m dot (n sum x^2-(sum x)^2)/(n sum x)&= (n sum x y sum x^2-sum x^2 sum x sum y)/(n(sum x^2)^2) $
      )
Now that $m$ is isolated, just simplify
#align(
      $ m &= (n sum x^2(n sum x y sum x^2 -sum x^2 sum x sum y))/(n(sum x^2)^2(n sum x^2 - (sum x)^2))\
      m&= (n^2 sum x y (sum x^2)^2 -n sum x sum y (sum x^2)^2)/(n(sum x^2)^2(n sum x^2 - (sum x)^2))\
      m &= (n sum x y- sum x sum y)/(n sum x^2- (sum x)^2) $
)


== Question (b):
  
Implement solve_least_squares_formula(x, y) in Python to calculate $b$ and $m$ directly from these formulas. Compare with solve_least_squares(x, y) on example data.

#pagebreak()

== Answer (b):
#box[```python
def solve_least_squares_formula(x, y):
    """
    Calculate b and m directly from explicit formulas.
    Returns: (intercept, slope)
    """
    N = len(x) # number of points in the data
    sum_x = np.sum(x)
    sum_y = np.sum(y)
    sum_xy = np.sum(x * y)
    sum_x_squared = np.sum(x**2)
    
    denominator = N * sum_x_squared - sum_x**2
    b = (sum_x_squared * sum_y - sum_x * sum_xy) / denominator
    m = (N * sum_xy - sum_x * sum_y) / denominator
    return b, m

# Method 1: QR Decomposition
matrix_b, matrix_m = solve_least_squares(x, y)
print(f"Matrix Method:")
print(f"  Intercept (b): {matrix_b:.8f}")
print(f"  Slope (m):     {matrix_m:.8f}")

# Method 2: Explicit Formula
formula_b, formula_m = solve_least_squares_formula(x, y)
print(f"\n Formula Method:")
print(f"  Intercept (b): {formula_b:.8f}")
print(f"  Slope (m):     {formula_m:.8f}")

# Calculate differences
matrix_formula_diff_b = abs(matrix_b - formula_b)
matrix_formula_diff_m = abs(matrix_m - formula_m)

print(f"\nDifferences:")
print(f"  Matrix vs Formula - Intercept: {matrix_formula_diff_b:.2e}")
print(f"  Matrix vs Formula - Slope:     {matrix_formula_diff_m:.2e}")

[OUTPUT]
Matrix Method:
  Intercept (b): 4.82718715
  Slope (m):     2.01379327

Formula Method:
  Intercept (b): 4.82718715
  Slope (m):     2.01379327

Differences:
  Matrix vs Formula - Intercept: 1.78e-15
  Matrix vs Formula - Slope:     4.44e-16
```] 

#pagebreak()
Timing:
#box(
  ```py
  # Timing comparison
  import time

  # iterations for calculating avg speed for methods
  iterations = 1000

  # Time  matrix method
  start = time.time()
  for _ in range(iterations):
      solve_least_squares(x, y)
  matix_time = (time.time() - start)/iterations

  # Time formula method
  start = time.time()
  for _ in range(iterations):
      solve_least_squares_formula(x, y)
  formula_time = (time.time() - start)/iterations

  print(f"\nTiming (avg iteration):")
  print(f"  Matrix method: {matix_time:.6f} seconds")
  print(f"  Formular method: {formula_time:.6f} seconds")
  print(f"  Speed ratio: {matix_time/formula_time:.2f}x")

[OUTPUT]
Timing (avg iteration):
  Matrix method: 0.000009 seconds
  Formular method: 0.000005 seconds
  Speed ratio: 1.79x
  ```
)
The two methods are nearly identical in computation (they roughly give the same answer). Notice that the formula method is faster, probably since it's just formula computing.





#pagebreak()
= Task 7: Comparing with Gradient Descent

== Question (a):
  
Implement gradient descent to minimize $f(b, m)$ by iteratively updating $b$ and $m$ using the partial derivatives.

== Answer (a):
#box[```python
def gradient_descent(x, y, b, m, learning_rate=0.01, max_iterations=1000):
    """
    Minimize f(b, m) using gradient descent.
    """
    epsilon = 1e-6
    
    for _ in range(max_iterations)
        dfdb = partial_derivative_b(x, y, b, m)
        dfdm = partial_derivative_m(x, y, b, m)
        
        # Check for too lage numbers instability (infiniy or div by 0)
        if not (np.isfinite(dfdb) and np.isfinite(dfdm)):
            return b, m
        
        if abs(dfdb) < epsilon and abs(dfdm) < epsilon:
            return b, m
        
        # Update parameters for next iteration
        b = b - learning_rate * dfdb
        m = m - learning_rate * dfdm
        
    # max iterations have been reached
    return b, m

```
]
#pagebreak()
== Question (b):
  
Compare results from solve_least_squares, solve_least_squares_formula, and gradient descent on the example dataset. Discuss any differences in convergence and accuracy.


== Answer (b):

#box[
  ```py
# keep low
learning_rate = 0.00001

ls_b, ls_m = solve_least_squares(x, y)
formula_b, formula_m = solve_least_squares_formula(x, y)
gd_b, gd_m = gradient_descent(x, y, 0, 0, learning_rate=0.00001, max_iterations=10000)

print("\nRegression Results:")
print(f"Least Squares:        b = {ls_b:.6f}, m = {ls_m:.6f}")
print(f"Explicit Formula:     b = {formula_b:.6f}, m = {formula_m:.6f}")
print(f"Gradient Descent:     b = {gd_b:.6f}, m = {gd_m:.6f}")

print("\nDifferences:")
print(f"LS vs Formula:        Δb = {abs(ls_b-formula_b):.2e}, Δm = {abs(ls_m-formula_m):.2e}")
print(f"LS vs GD:             Δb = {abs(ls_b-gd_b):.2e}, Δm = {abs(ls_m-gd_m):.2e}")
print(f"Formula vs GD:        Δb = {abs(formula_b-gd_b):.2e}, Δm = {abs(formula_m-gd_m):.2e}")

[OUTPUT]
Regression Results:
Least Squares:     b = 4.827187, m = 2.013793
Explicit Formula:     b = 4.827187, m = 2.013793
Gradient Descent:     b = 4.796321, m = 2.018434

Differences:
LS vs Formula:        Δb = 1.78e-15, Δm = 4.44e-16
LS vs GD:             Δb = 3.09e-02, Δm = 4.64e-03
Formula vs GD:        Δb = 3.09e-02, Δm = 4.64e-03
  ```
]
#pagebreak() 
To increase the accuracy, I should adjust the learning rate for the gradient descent, so that the differences are minimal. 
#box[
  ```py
# keep low
learning_rate = 0.0001

# calculation

[OUTPUT]
Regression Results:
Least Squares:     b = 4.827187, m = 2.013793
Explicit Formula:     b = 4.827187, m = 2.013793
Gradient Descent:     b = 4.827187, m = 2.013793

Differences:
LS vs Formula:        Δb = 1.78e-15, Δm = 4.44e-16
LS vs GD:             Δb = 2.01e-08, Δm = 3.03e-09
Formula vs GD:        Δb = 2.01e-08, Δm = 3.03e-09
  ```
]
The results show that by adjusting the step size (learning_rate) in gradient descent, we can achieve higher accuracy.

It is important to note that gradient descent only approaches the true values for $b$ and $m$, and cannot surpass the exact solution (least squares method). Gradient descent iteratively moves closer to the minimum, but its accuracy is limited by the learning rate, the number of iterations, and numerical precision.

Also, any difference between the least squares (LS) and true formula methods should theoretically be zero, as both solve the same system. However, small discrepancies may occur due to internal rounding and/or floating-points in the computations.

#pagebreak()
= Task 8: Differentiation Methods Analysis

== Question (a):
  
Based on your study of PartI_differentiation.ipynb and PartII_autograd.ipynb, discuss the advantages and limitations of numerical differentiation, symbolic differentiation, and automatic differentiation.

== Answer (a):





*Note* that there are not just one of these method which definitivly bette than the others. They all have different advatnges as well as limitations.

+ Numerical Differentiation:
The big advangtage of numerical differentiation, is that "it does not matter at all how the function was calculated - only the final values of it!" @PartI_differentiation. This is because of the ```py np.gradient(f)``` function, which makes numerical differentiation very easy and intuitive to work with.

On the other hand, as explained in the ```PartI_differentiation```@PartI_differentiation, you might lose some accuracy with numerical differentiation. Also since numerical values cant handle "jumps" of the derivative. a good example is $f(x)=abs(x)$,
#figure(
  caption: [$f(x)=abs(x)$ shown as a graph @PartI_differentiation,]
)[
  #image("images/image.png")
]

#pagebreak()
+ Symbolic differentiation
  
  Symbolic differentiation uses exact representation of mathicatical objects @PartI_differentiation. Take the example ```py sp.sqrt(2)``` will not be represented as $1.41421356237$ but exactly as $sqrt(2)$

  This way, calculations are more precise since no rounding is being done while calculating steps in the calculation of the result.

  Besides the jump in the derivative (e.g. $f(x)=abs(x)$)
  The main limitation of symbolic differentiation, is the computation time. Especially larger and more complex functions can get very slow.
  This is known as the *expression swell* @PartI_differentiation.

+ Automatic differentiation
  
  Automatic differentiation offer a vastly differet and easier approch, with modern libaries like ```py Autograd``` and ```py JAX```
  @PartII_autograd. Take ```py JAX```, which is a libary full of usefull functions for differentiation, including the ```py jax.grad()``` function which can differentiate whole python functions

  Automatic differentiation can be limiting in the sense that you cant really debug somthing like the ```py jax.grad()```
  or easily inspect the intermediate steps of the computation graph. Errors or unexpected results may be hard to trace, especially in complex models, because the differentiation happens behind the scenes.
== Question (b):
  
Summarize the connection of multivariate chain rule and backpropagation, and discuss why backward differentiation is efficient for optimizing neural networks.

== Answer (b):

Take the concept of a neural network as a vector (input) being transformed through layers until a loss is computed.

We can describe this as:
$ v_0->f_1(v_0)->f_2(v_1)->dots->f_L (v_(L-1))->"Loss" $

Where each layer might look something like this
$ f_L (v_L)=sigma(W_L dot v_(L-1) + b_L) $

This means that $ "Loss" = f_L (f_(L-1)(f_(L-2)(dots f_1(v_0)))) $

To find the change of loss we now need to compute $(partial L)/(partial v_0)$ 

For this we use the multivariate chain rule
$ (partial L)/(partial v_0)=(partial L)/(partial f_(L)) dot (partial f_(L))/(partial f_(L-1)) dot (partial f_(L-1))/(partial f_(L-2)) dot dot dot (partial f_(1))/(partial v_0) $
Now with backprobagration we can go back though the layers and change the weights to reduce the loss

#pagebreak()
= Conclusion
In this project, I explored several approaches to regression and differentiation in Python. Each method demonstrated distinct advantages and limitations in terms of speed, accuracy, and usability. I also examined how these mathematical concepts relate to machine learning and neural networks. Overall, this work provided valuable insights into the strengths of different techniques and deepened my understanding of their practical applications.

#bibliography("references.bib")

