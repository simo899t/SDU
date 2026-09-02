import math
def permutations(a,b):
    return math.factorial(a)/(math.factorial(a-b))

def combinations(a,b):
    return permutations(a,b)/math.factorial(b)

print("combinations")
#combinations of a and b
print(combinations(8,3))
print(combinations(7,3) + combinations(7,2))

print("permutaions")
#permutations of a and b
print(permutations(5,2))
print(permutations(25,2))

print("truth determinations")
#truth determinations

def fact():
    for n in range(1,500):
        for k in range(1,500):
            if n > k:
                print(permutations(n,k)*math.factorial((n-k+1)) == math.factorial(n))

#print(fact())

def count():
    for i in range(1,500):
        print(permutations(i,i) == i**2)

print(permutations(2,2))
print(10)
print(permutations(30,3) == combinations(30,3)/6)

print(combinations(5,0) + 
      combinations(5,1) + 
      combinations(5,2) + 
      combinations(5,3) + 
      combinations(5,4) + 
      combinations(5,5))
print(math.factorial(5))
print(permutations(17,5)*math.factorial((17-5+1)) == math.factorial(17))