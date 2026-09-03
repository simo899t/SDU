def boolian_product(A: list[list[int]],B:list[list[int]]) -> list[list[int]]:
    """Returns the boolian product of matrix A and matrix B"""
    return [[sum(a*b for a,b in zip(X_row,Y_col)) for Y_col in zip(*B)] for X_row in A]

A = [[0, 2, 7, 3], [2, 0, 999, 6], [7, 999, 0, 999], [3, 6, 999, 0]]
B = [[0, 2, 7, 3], [2, 0, 999, 6], [7, 999, 0, 999], [3, 6, 999, 0]]

C = [[1, 0], [1, 1]]
D = [[1, 1], [0, 1]]
print(boolian_product(C,D))