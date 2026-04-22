import numpy as np

mat1 = np.matrix([[ 2, 5,-3, 0],
                  [ 0, 6, 0,-4],
                  [-1,-3, 0,-4],
                  [ 5, 0, 0, 3]])
mat2 = np.matrix([[-2, 0],
                  [ 4, 6]])
mat3_len = mat1.shape[0] - mat2.shape[0] + 1  # (4-2+1) = 3

mat3 = np.zeros((mat3_len, mat3_len), dtype=int)

for i in range(mat3_len):
    for j in range(mat3_len):
        for k in range(mat2.shape[0]):
            for l in range(mat2.shape[1]):
                mat3[i, j] += mat1[i+k, j+l] * mat2[k, l]

print(mat3)
