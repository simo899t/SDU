from functools import reduce
from math import *

x = [1,2,3]

y = [1,2,3]

def centroid(x:list[int], y:list[int]):
    """Finds centroid of a x and y coordinates"""
    result = [reduce(lambda x,y: x + y, x)/len(x)] + [reduce(lambda x,y: x + y, y)/len(y)]
    return result
    
def TD2(x:list[int], y:list[int]):
    """Determends the TD^2 value of x and y coordinates"""
    distances = []
    i = 0
    while i < len(x):
            distances = distances + [sqrt((x[i] - centroid(x,y)[0])**2 + (y[i] - centroid(x,y)[1])**2)]
            i = i + 1
    return reduce(lambda x,y: x + y, distances)



print(centroid(x,y))
print(TD2(x,y))

