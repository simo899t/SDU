# ===============================
# Experiment Results Log
#
# Best Accuracy:
#   transform:  ToTensor, transforms.Normalize((mean),), (sdt,))
#   h_layers:   6
#   width:      512
#   activation: ReLU
#   dropout:    0.2
#   optimizer:  SGD
#   lr:         0.1 
#   n_epoch:    20        
#   scheduler:  StepLR(optimizer, step_size=10, gamma=0.1)
#   Accuracy:   98.52%
# ===============================

import torch
import torch.nn as nn
from torch import optim
from torchvision import datasets, transforms
import seaborn as sns

# Download the MNIST dataset (first without transform to compute mean/std)
raw_train = datasets.MNIST(root='./data', train=True, download=True, transform=transforms.ToTensor())
mean = raw_train.data.float().mean() / 255
std = raw_train.data.float().std() / 255

# Now use these values in Normalize
transform = transforms.Compose([
    transforms.ToTensor(),
    transforms.Normalize((mean.item(),), (std.item(),))
])
train_dataset = datasets.MNIST(root='./data', train=True, download=True, transform=transform)
test_dataset = datasets.MNIST(root='./data', train=False, download=True, transform=transform)
train_loader = torch.utils.data.DataLoader(dataset=train_dataset, batch_size=64, shuffle=True)
test_loader = torch.utils.data.DataLoader(dataset=test_dataset, batch_size=64, shuffle=False)


break_on_first_solution = False

class MyNetwork(nn.Module):
    def __init__(self):
        super(MyNetwork, self).__init__()
        self.fc1 = nn.Linear(in_features=784, out_features=512)
        self.fc2 = nn.Linear(in_features=512, out_features=512)
        self.fc3 = nn.Linear(in_features=512, out_features=512)
        self.fc4 = nn.Linear(in_features=512, out_features=512)
        self.fc5 = nn.Linear(in_features=512, out_features=512)
        self.fc6 = nn.Linear(in_features=512, out_features=10)
        self.activation = nn.ReLU()                 # 98.32%
        # self.activation = nn.LeakyReLU(0.001)     # 98.32%
        # self.activation = nn.Sigmoid()            # 97.56%
        # self.activation = nn.Tanh()               # 98.08%
        # self.activation = nn.ELU()                # 98.17%
        # self.activation = nn.GELU()               # 98.25%
        # self.activation = nn.SELU()               # 98.28%
        # self.activation = nn.Softplus()           # 97.93%
        # self.activation = nn.Hardtanh()           # 98.00%
        # self.activation = nn.SiLU()               # 98.28%

        self.dropout = nn.Dropout(0.2)

    
    def forward(self, x):
        x = x.view(-1, 28*28)
        x = self.dropout(self.activation(self.fc1(x)))
        x = self.activation(self.fc2(x))
        x = self.activation(self.fc3(x))
        x = self.activation(self.fc4(x))
        x = self.activation(self.fc5(x))
        x = self.fc6(x)
        return x

net = MyNetwork()



# learning rates
SGD_lr = 0.1
Adam_lr = 0.01
AdamW_lr = 0.001
RMSprop_lr = 0.01
Adagrad_lr = 0.01
Adadelta_lr = 0.01
NAdam_lr = 0.01
ASGD_lr = 0.01

n_epochs = 50

_avg = []
optimizer = optim.SGD(net.parameters(), lr=SGD_lr)
# optimizer = optim.Adam(net.parameters(), lr=learning_rate)
# optimizer = optim.RMSprop(net.parameters(), lr=learning_rate)
# optimizer = optim.Adagrad(net.parameters(), lr=learning_rate)
# optimizer = optim.Adadelta(net.parameters(), lr=learning_rate)
# optimizer = optim.NAdam(net.parameters(), lr=learning_rate)
# optimizer = optim.ASGD(net.parameters(), lr=learning_rate)
# optimizer = optim.AdamW(net.parameters(), lr=AdamW_lr, weight_decay=0.0001)

from torch.optim.lr_scheduler import StepLR

optimizer = optim.SGD(net.parameters(), lr=SGD_lr)
scheduler = StepLR(optimizer, step_size=5, gamma=0.8)  # Decays LR by 0.1 every 10 epochs




criterion = nn.CrossEntropyLoss()

all_epoch_losses = []
for i in range(n_epochs):
    epoch_losses = []
    for batch in train_loader:
        x, y = batch
        y_hat = net(x)
        loss = criterion(y_hat, y)
        loss.backward()
        optimizer.step()
        optimizer.zero_grad()
        epoch_losses.append(loss)
    avg_loss = sum(epoch_losses) / len(epoch_losses)
    all_epoch_losses.append(avg_loss)
    scheduler.step()

    print(f"Epoch {i} avg loss: {avg_loss.item()}")


    # Evaluate on test set and print accuracy for each epoch
    net.eval()
    correct = 0
    total = 0
    with torch.no_grad():
        for x_test, y_test in test_loader:
            y_hat = net(x_test)
            preds = torch.argmax(y_hat, dim=-1)
            correct += (preds == y_test).sum().item()
            total += y_test.size(0)
    acc = correct / total
    print(f"Epoch {i} test accuracy: {acc * 100:.2f}%")
    _avg.append(acc)
    if break_on_first_solution and (acc - 1.0) == 0:
        break