from matrix import *

A = [[1,2,3],
     [1,2,3],
     [1,2,3]]

B = [[1,0,1,1],
     [0,1,1,1],
     [1,1,1,1],
     [1,0,0,0]]

C = [[1,0,1,1],
     [0,1,1,1]]

D = [[0,1],
     [0,1],
     [0,1],
     [1,0]]

E = [[1,0,4],
     [2,1,1],
     [3,1,0],
     [0,2,2]]

F = [[1,2],
     [3,4],
     [5,6]]

G = [[0,1,0,0,1,0],
     [1,0,1,0,1,0],
     [0,1,0,0,0,0],
     [0,0,0,0,1,1],
     [1,1,0,1,0,0],
     [0,0,0,1,0,0]]

H = [[2,0],
     [0,2]]

"""matrixAddition(A,B)
>>>[[1, 1, 1, 1], [0, 2, 2, 1], [1, 2, 2, 2], [2, 0, 1, 0]]"""

#print(matrixAddition(A,B))

"""matrixAddition(A,C)
Matrices, A and B, are not of equal size"""

print(matrixMultiplication(  [[1, 2, 7, 3], 
                              [2, 0, 999, 6], 
                              [7, 999, 0, 999], 
                              [3, 6, 999, 0]],
                      
                             [[0, 2, 7, 3], 
                              [2, 0, 999, 6], 
                              [7, 999, 0, 999], 
                              [3, 6, 999, 0]]))

print("1")
print(matrixMultiplication(  [[1, 2], 
                              [3, 4]],

                             [[1, 2], 
                              [1, 4]]))
        


print("2")
print(matrixMultiplication(  [[1, 2], 
                              [3, 4],
                              [5, 6]],
        
                             [[1,2,3],
                              [1,2,3]]))

print("3")
print(matrixMultiplication(  [[1,2,3],
                              [1,2,3]],
                              
                             [[1, 2], 
                              [3, 4],
                              [5, 6]]))
print("here")
print(matrixMultiplication(matrixMultiplication(  [[1, 2], 
                              [3, 4],
                              [5, 6]],
        
                             [[1,2,3],
                              [1,2,3]]), matrixMultiplication(  [[1,2,3],
                              [1,2,3]],
                              
                             [[1, 2], 
                              [3, 4],
                              [5, 6]])))

"""matrixMultiplication(A,A)
>>> [[0, 1, 1, 1], [1, 2, 2, 2], [2, 2, 2, 2], [2, 1, 2, 2]]"""

"""matrixMultiplication(A,C)
Matrices are not multiplicable"""

"""matrixMultiplication(C,A)
>>> [[1, 2, 2, 1], [1, 2, 3, 1]]"""

"""kLengthWalks(G,10,5,3)
>>> 140"""
from math import *
print(min(2+3,0+6,inf+inf,6+6))