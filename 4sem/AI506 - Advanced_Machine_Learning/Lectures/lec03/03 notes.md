Activation functions
- softmax
- ELU
- SELU
- softplus
- ReLU
	- $x = max(0,x_i)$
- sigmoid
- Tanh

![[Pasted image 20260211153158.png]]


Common output units
- **Linear units:** no non-linearity (used for Regressions)
- **Sigmoid units:** each individual output is between 0 and 1 (used for many-out-of-K classification)
	- can assign multiple classes
- **Softmax units:** each individual output is between 0 and 1 and all outputs together sum up to 1 (e.g., used for one-out-of-K classification)
	- can only choose one class