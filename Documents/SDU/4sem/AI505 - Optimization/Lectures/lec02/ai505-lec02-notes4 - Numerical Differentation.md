Neighboring points are used to approximate the derivative

Such as: 
$$f(x) approx underbrace(lim_(h->0) (f(x+h)-f(x))/h, "forward difference") = underbrace(lim_(h->0) (f(x+(h)/2)-f(x-(h)/2))/h, "central difference") = underbrace(lim_(h->0) (f(x)-f(x-h))/h, "backward difference")$$
![[Pasted image 20260210113235.png]]
- The O(n) complexity of numerical differentiation for a gradient in $n$ dimensions is the main obstacle to its usefulness in machine learning, where n can be as large as millions or billions in *state-of-the-art* deep learning models