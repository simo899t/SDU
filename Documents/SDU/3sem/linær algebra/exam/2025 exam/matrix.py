import numpy as np

# Define the matrix with proper syntax
mat = np.array([
    [3, -1],
    [1, 1]
])

print('Matrix:')
print(mat)
# Calculate determinant
det_mat = np.linalg.det(mat)
print(f'Determinant: {det_mat}')

# Find eigenvalues and eigenvectors
eigenvalues, eigenvectors = np.linalg.eig(mat)

print(f'\nEigenvalues: {eigenvalues}')

print(eigenvectors)
# Check if matrix is diagonalizable
# A matrix is diagonalizable if the eigenvector matrix is invertible
det_eigenvectors = np.linalg.det(eigenvectors)
print(f'Determinant of eigenvector matrix: {det_eigenvectors}')

if abs(det_eigenvectors) > 1e-10:
    print('Matrix IS diagonalizable')
else:
    print('Matrix is NOT diagonalizable')
