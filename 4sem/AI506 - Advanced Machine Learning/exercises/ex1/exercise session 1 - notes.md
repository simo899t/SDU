#### Linear layer
![[Pasted image 20260212123849.png]]
- inplace = true, means that we modify the input tensor directly without creating a new tensor for the output
![[Pasted image 20260212123931.png]]
#### Data Loader
![[Pasted image 20260212124002.png]]
##### Simple data loader
![[Pasted image 20260212124301.png]]

#### Save and load models
- Saving: You give it the parameters of a model using MyModel.state_dict() and a path of where you want to save the model *including extension .pth*
![[Pasted image 20260212124503.png]]
- Loading: 
	- Initialize the network class
	- Load a pretrained network by giving it the path including extension
	- Ff you then want to use the model without training, you need to set it to evaluation mode by running MyModel.eval()
![[Pasted image 20260212124636.png]]

#### Optimizer
Most common optimizers are
- SGD (Stochastic gradient descent)
- ADAM (Adaptive Moment Estimation)

#### Evaluating the model
- Metrics (F1, Precision, Recall, Accuracy, ROC etc.)
- ![[Pasted image 20260212131257.png]]
