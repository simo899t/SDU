import numpy as np

#   Exercise 1
# Write a list of the first 100 numbers
# in which any number divisible by three is replaced by the word “fizz”
# and any divisible by five by the word “buzz”.  
# Numbers divisible by both become “fizz buzz”.  
# Can you create a numpy array containing this list?
def fizz_buzz():
    result = []
    for i in range(1, 101):
        if i % 3 == 0 and i % 5 == 0:
            result.append("fizzbuzz")
        elif i % 3 == 0:
            result.append("fizz")
        elif i % 5 == 0:
            result.append("buzz")
        else:
            result.append(i)
    return np.array(result)

#   Exercise 2 
# Data TypesRevise the difference between the main data types in Python:  
# list, tuples, dictionaries and sets.  
# Write an example for each of them in which you define and initialize a variable 
# for each type and then print the content looping through the elements of the variable.
# Numpy arrays can only contain data of the same type hence we cannot create an array 
# from a list containing both numbers and strings.

def data_types_examples():
    # List
    my_list = [1, 2, 3, 4, 5]
    print("List:")
    for item in my_list:
        print(item)

    # Tuple
    my_tuple = (1, 2, 3, 4, 5)
    print("\nTuple:")
    for item in my_tuple:
        print(item)

    # Dictionary
    my_dict = {'a': 1, 'b': 2, 'c': 3}
    print("\nDictionary:")
    for key, value in my_dict.items():
        print(f"{key}: {value}")

    # Set
    my_set = {1, 2, 3, 4, 5}
    print("\nSet:")
    for item in my_set:
        print(item)


#   Exercise 3 Python: One liner quizzes
# Write a one line Python code for the following tasks:
# a)  Construct the set S = {x ∈ R | 50 ≥ x ≥ 0 ∧ x mod 3 ≡ 1}

def one_liner_quiz_a():
    return list(x for x in range(51) if x % 3 == 1)

# b) Using list comprehension make a list for{(i, j)|i∈{1,2,3,4}, j∈{5,7,9}}

def one_liner_quiz_b():
    return [(i, j) for i in {1, 2, 3, 4} for j in {5, 7, 9}]

# c) Calculate the inverse of a function 
# or the index function for an invertible function 
# (ie, bijective = injective + surjective) given in form of a dictionary.

func = {'a': 10, 'b': 2, 'c': 8}

def one_liner_quiz_c(func_dict):
    return {v: k for k, v in func_dict.items()}

# d) What is the result of the following lines?
def one_liner_quiz_d1():
    return list(map(lambda x: x%3, range(5)))
#    >>>> [0, 1, 2, 0, 1]
def one_liner_quiz_d2():
    return list(filter(lambda x: x%2 == 0, range(5)))
#    >>>> [0, 2, 4]

# Exercise 4 Matrix Calculus in basic Python
# The basic data structures in Python are lists, tuples, sets and dictionaries.  
# Vectors and matrices can be implemented in Python as lists.  How?
#     use lists to implement vectors or matrices


# a) Generate a couple of numerical examples for vectors and matrices.  
# Experiment with the operators+and*.  Do they yield the same result as expected from linear algebra?

# b) Write a function for the sum of two vectors using list comprehension.

scalar = 3
vector1 = [1, 2, 3]
vector2 = [4, 5, 6]
matrix1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
matrix2 = [[9, 8, 7], [6, 5, 4], [3, 2, 1]]

def vector_addition(v1, v2):
    if len(v1) != len(v2):
        raise ValueError("Vectors must be of the same size")
    return [x + y for x, y in zip(v1, v2)] 

# c) Write a function for the multiplication of a vector by a scalar.
def scalar_multiplication_vector(scalar, vector):
    return [scalar * x for x in vector]

# d) Write a function for the sum of two matrices using list comprehension.
def matrix_addition(m1, m2):
    if len(m1) != len(m2) or len(m1[0]) != len(m2[0]):
        raise ValueError("Matrices must be of the same size")
    return [[x + y for x, y in zip(row1, row2)] for row1, row2 in zip(m1, m2)]

# e) Write a function for the multiplication of a matrix by a scalar.
def matrix_scalar_multiplication(scalar, matrix):
    return [[scalar * x for x in row] for row in matrix]

# f) Write a function for the multiplication of two matrices not necessarily square.  
def matrix_multiplication(m1, m2):
    if len(m1[0]) != len(m2):
        raise ValueError("Incompatible matrix sizes")
    return [[sum(x * y for x, y in zip(row, col)) for col in zip(*m2)] for row in m1]

#   Exercise 5* Matrix Calculus in numpy and scipy
# The modules numpy and scipy make available another data structure in Python, the ’array’ type.  
# This exercise guides you to the discovery of how operators are overloaded for the ’array’ type module.
# Generate in Python two matrices A and B of size 3×2 and 2×4, respectively, 
# made of integer numbers randomly drawn from the interval  [1, . . . ,10].   
# Calculate the following results, first by hand and then checking the correctness of your answer in Python numpy:

A = np.matrix(np.random.randint(1, 11, size=(3, 3)))
B = np.matrix(np.random.randint(1, 11, size=(3, 2)))
C = np.matrix(np.random.randint(1, 11, size=(4, 3)))

# a1) A + B
def excercise_5_a1(A, B):
    try:
        return np.add(A, B)
    except ValueError as e:
        return str(e)

# a2) A - B
def excercise_5_a2(A, B):
    try:
        return np.subtract(A, B)
    except ValueError as e:
        return str(e)

# b) A * B
def excercise_5_b(A, B):
    try:
        return np.matmul(A, B)
    except ValueError as e:
        return str(e)

# c) A / B
def excercise_5_c(A, B):
    try:
        return np.divide(A, B)
    except ValueError as e:
        return str(e)

# Exercise 6* Matrix Operations
# For some aribtrary dimension and some random numbers in the matrices:
# a)  Construct an array of zeros
def excercise_6_a(a, b):
    return np.zeros((a, b))

# b)  Concatenate an identity matrix to a matrix
def excercise_6_b(matrix):
    try:
        if matrix.shape[0] != matrix.shape[1]:
            raise ValueError("Matrix must be square to concatenate with identity matrix")
        identity = np.eye(matrix.shape[0])
        return np.concatenate((matrix, identity), axis=1)
    except ValueError as e:
        return str(e)
    
# c)  Insert a row in between other two
def excercise_6_c(matrix, row, index):
    result = np.copy(matrix)
    try:
        if index < 0 or index > result.ndim:
            raise IndexError("Index out of bounds")
        return np.insert(result, index, row, axis=0)
    except IndexError as e:
        return str(e)

# d)  Print the dimensions of an array
def excercise_6_d(matrix):
    return matrix.shape[0]

# e)  Multiply matrices
def excercise_6_e(A, B):
    try:
        return np.matmul(A, B)
    except ValueError as e:
        return str(e)
# f)  Print the matrix transpose
def excercise_6_f(matrix):
    return np.transpose(matrix)

# g)  Print the rank of a matrix
def excercise_6_g(matrix):
    return np.linalg.matrix_rank(matrix)



# Exercise 7* Matrix InverseCalculate the inverse of these two matrices:
D = [[-1, 3/2], [1, -1]]
E = [[-1, 3/2], [2/3, -1]]
def excercise_7(matrix):
    try:
        return np.linalg.inv(matrix)
    except np.linalg.LinAlgError as e:
        return str(e)

# Exercise 8* Indexing and slices
# Construct an array of dimension 3×3 containing the first 9 natural numbers.  
matrix2 = np.matrix(np.arange(1, 10).reshape(3, 3))
# Remember that indices start at 0.
# a)  print the element at (0,0)
def excercise_8_a(matrix):
    print(matrix[0, 0])
# b)  print all rows starting from the second
def excercise_8_b(matrix):
    print(matrix[1:, :])
# c)  print the second row of the matrix
def excercise_8_c(matrix):
    print(matrix[1, :])
# d)  print a vector filled by the 3rd column of the matrix
def excercise_8_d(matrix):
    print(matrix[:, 2])
# e)  print the type of the data contained in the matrix2
def excercise_8_e(matrix):
    print(matrix.dtype)

def main():
    # print(fizz_buzz())
    # data_types_examples()
    # print(one_liner_quiz_a())
    # print(one_liner_quiz_b())
    # print(one_liner_quiz_c(func))
    # print(one_liner_quiz_d1())
    # print(one_liner_quiz_d2())
    # print(vector_addition(vector1, vector2))
    # print(scalar_multiplication_vector(scalar, vector1))
    # print(matrix_addition(matrix1, matrix2))
    # print(matrix_scalar_multiplication(scalar, matrix1))
    # print(matrix_multiplication(matrix1, matrix2))
    # print(str(A))
    # print(str(B))
    # print(excercise_5_a(A, B))
    # print(excercise_5_b(A, B))
    # print(excercise_5_b(E,D))
    # print(excercise_6_a(3,3))
    # print(excercise_6_b(A))
    # print(excercise_6_c(A, np.array([1, 2, 3]), 1))
    # print(excercise_6_d(A))
    # print(excercise_7(E))
    # print(excercise_7(D))
    pass

if __name__ == "__main__":
    main()