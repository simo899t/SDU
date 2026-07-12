#### Tensor
An array of numbers (like vectors or matrices) but with an arbitrary number of axes
Note that this is denoted as the rank of a tensor. 

##### example: 
multiple images with colorcodes
$$T in RR^("batchsize"times h times w times c)$$
#### Sum rule
$$P(X=x_i) = sum_(j=1)^L P(X=x_i, Y = y_i)$$
$$P(X)=sum_Y P(X,Y)$$
#### Product rule
$$P(X, Y) = P(Y|X)P(X)$$
Bayes Theorem

$$underbrace(P(Y|X),"posterior") = (overbrace(P(X|Y), "likelihood")overbrace(P(Y),"prior"))/underbrace(P(X),"evidence")$$
#### Entropy
Degree of uncertainty

Example, a fair dice, has high uncertainty (because all events er equally likely)
on the other hand, a bias dice, might have higher uncertainty