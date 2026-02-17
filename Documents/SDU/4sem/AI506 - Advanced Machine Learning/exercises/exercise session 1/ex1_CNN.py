# ===============================
# Experiment Results Log
#
# Best Accuracy:
#   transform:  ToTensor, transforms.Normalize((mean),), (sdt,))
#   h_layers:   5
#   activation: ReLU
#   dropout:    0.25
#   optimizer:  Adam
#   lr:         0.001 
#   n_epoch:    10        
#   scheduler:  none
#   Accuracy:   99.17%
# ===============================
import torch
import torch.nn as nn
from torch import optim
from torchvision import datasets, transforms

print("loading data...")

# Use GPU if available
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

# Hardcoded MNIST mean and std for speed
mean = 0.1307
std = 0.3081

transform = transforms.Compose([
    transforms.ToTensor(),
    transforms.Normalize((mean,), (std,))
])

train_dataset = datasets.MNIST(root='./data', train=True, download=True, transform=transform)
test_dataset = datasets.MNIST(root='./data', train=False, download=True, transform=transform)
train_loader = torch.utils.data.DataLoader(dataset=train_dataset, batch_size=128, shuffle=True)
test_loader = torch.utils.data.DataLoader(dataset=test_dataset, batch_size=128, shuffle=False)

class SimpleCNN(nn.Module):
    def __init__(self):
        super(SimpleCNN, self).__init__()
        self.conv1 = nn.Conv2d(1, 32, kernel_size=3, padding=1)
        self.conv2 = nn.Conv2d(32, 64, kernel_size=3, padding=1)
        self.pool = nn.MaxPool2d(2, 2)
        self.dropout = nn.Dropout(0.25)
        self.fc1 = nn.Linear(64 * 14 * 14, 128)
        self.fc2 = nn.Linear(128, 10)
        self.relu = nn.ReLU()

    def forward(self, x):
        x = self.relu(self.conv1(x))
        x = self.pool(self.relu(self.conv2(x)))
        x = self.dropout(x)
        x = x.view(-1, 64 * 14 * 14)
        x = self.relu(self.fc1(x))
        x = self.fc2(x)
        return x


net = SimpleCNN().to(device)

optimizer = optim.Adam(net.parameters(), lr=0.001)
criterion = nn.CrossEntropyLoss()
n_epochs = 10

print("start training and evalutation")

for epoch in range(n_epochs):
    net.train()
    epoch_loss = 0
    for x, y in train_loader:
        x, y = x.to(device), y.to(device)
        y_hat = net(x)
        loss = criterion(y_hat, y)
        loss.backward()
        optimizer.step()
        optimizer.zero_grad()
        epoch_loss += loss.item()
    print(f"Epoch {epoch} avg loss: {epoch_loss / len(train_loader):.4f}")

    # Evaluate
    net.eval()
    correct = 0
    total = 0
    with torch.no_grad():
        for x_test, y_test in test_loader:
            x_test, y_test = x_test.to(device), y_test.to(device)
            y_hat = net(x_test)
            preds = torch.argmax(y_hat, dim=1)
            correct += (preds == y_test).sum().item()
            total += y_test.size(0)
    acc = 100 * correct / total
    print(f"Test accuracy: {acc:.2f}%")

print("finished!")
