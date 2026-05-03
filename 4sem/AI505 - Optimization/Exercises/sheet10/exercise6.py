
import numpy as np
import scipy.minimize as min


A = np.matrix([43.5, 47.1, 48.4, 38.2],
              [45.5, 42.1, 49.6, 36.8],
              [43.4, 39.1, 42.1, 43.2],
              [46.5, 44.1, 44.5, 41.2],
              [46.3, 47.8, 50.4, 37.2])

c = np.array([])

min.linprog(c,)