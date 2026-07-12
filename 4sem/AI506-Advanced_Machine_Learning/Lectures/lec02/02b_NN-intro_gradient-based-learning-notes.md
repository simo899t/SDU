#### Activation function for NN
$$sigma(X^T W+b) quad  ==>quad sigma((sum x_i w_i)+b)$$
![[Pasted image 20260205113749.png]]
where each transformation from one layer to the next, requires a vector or weights
#### Gradient based learning
Update the model parameters following the steepest slope of the loss function
We use gradient descent
$$f(x+epsilon) approx f(x) + epsilon nabla f(x)$$so with loss function $$1/N sum_X nabla_theta J(theta,x_i,y_i) = nabla_theta J(theta,X,y) $$then $$theta_("new")=theta_("old") - epsilon nabla_theta J(theta,X,y)$$![[Pasted image 20260205115105.png]]