from functools import reduce 

class CI: 
  
  """
  cX=a (mod m)
  """
  def __init__(self, c, a, m):
    self.c = c%m
    self.a = a%m
    self.m = m
    
  def solve(self, x):
    return (x*self.c)%self.m == self.a
    
def find(cil):
  espacio = reduce(lambda acc, ci: acc*ci.m, cil, 1)+1
  for x in range(0, espacio):
    if reduce(lambda acc, ci: acc and ci.solve(x), cil, True):
      return x

def next_solution(x,list):
    return x + reduce(lambda x,y: x*y, list)

def solutions_from_to(list,a,b):
    return (b-a)//reduce(lambda x,y: x*y, list)

x = find([CI(1, -12, 13), CI(1, 35, 10)])
modlist = [13,10]

print("Solution is: " + str(x))
print("Next solution is: " + str(next_solution(x,modlist)))
print(str(solutions_from_to(modlist,-100,100)) + " solutions from a to b")

print(103//30)
print(((12%2) == (3%2) and (12%5 == 7%5)))