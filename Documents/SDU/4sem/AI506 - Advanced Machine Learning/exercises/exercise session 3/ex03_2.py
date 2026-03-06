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
import torch.optim as optim
from torchvision import datasets, transforms
import seaborn as sns
import matplotlib.pyplot as plt
from torch.optim.lr_scheduler import StepLR
import os
os.makedirs('outputs', exist_ok=True)

# Download the MNIST dataset
transform = transforms.ToTensor()
train_dataset = datasets.MNIST(root='./data', train=True,
download=True, transform=transform)
test_dataset = datasets.MNIST(root='./data', train=False,
download=True, transform=transform)
train_loader = torch.utils.data.DataLoader(dataset=train_dataset,
batch_size=64, shuffle=True)
test_loader = torch.utils.data.DataLoader(dataset=test_dataset,
batch_size=64, shuffle=False)

# size of dimentions of dataset
size_of_dataset = train_dataset.data.size()
print("Size of the dataset dimensions:", size_of_dataset)

# distribution of pred(y)
counts = torch.bincount(train_dataset.targets)

# list for the x-axis [0..9]
n = counts.size(0)
x_axis = range(n)

optimum = counts.sum().item() / n

error_pct = (counts.float() - optimum).abs().mean().item() / optimum * 100

plt.bar(x_axis, counts)
plt.axhline(y=optimum, color='red', linestyle='--', label=f'Optimum ({optimum:.0f}) — error: {error_pct:.2f}%')
plt.xlabel('Digit')
plt.ylabel('Count')
plt.title('MNIST digit distribution')
plt.xticks(x_axis)
plt.legend()
plt.show(block=False)
# plt.savefig()


break_on_first_solution = True

class MyNetwork(nn.Module):
    def __init__(self):
        super(MyNetwork, self).__init__()
        self.fc1 = nn.Linear(64*14*14,100)
        self.conv1 = nn.Conv2d(1,32, kernel_size=3, padding=1)
        self.conv2 = nn.Conv2d(32,64, kernel_size=3, padding=1)
        self.fc2 = nn.Linear(in_features=100, out_features=10)
        self.pool = nn.MaxPool2d(2,2)
        self.activation = nn.ReLU()
        self.dropout = nn.Dropout(0.2)

    def forward(self, x):
        x = self.activation(self.conv1(x))
        x = self.activation(self.conv2(x))
        x = self.pool(x)
        x = x.view(x.size(0),-1)
        x = self.dropout(self.activation(self.fc1(x)))
        x = self.fc2(x)
        return x


def plot_conv1_filters(model, title):
    filters = model.conv1.weight.data  # shape: (32, 1, 3, 3)
    _, axes = plt.subplots(4, 8, figsize=(12, 6))
    for i, ax in enumerate(axes.flat):
        ax.imshow(filters[i, 0].numpy(), cmap='gray')
        ax.axis('off')
    plt.suptitle(f'conv1 filters — {title}')
    plt.tight_layout()
    plt.savefig(f'outputs/filters_{title}.png')
    plt.show()


def plot_conv2_filters(model, title):
    filters = model.conv2.weight.data  # shape: (64, 32, 3, 3)
    _, axes = plt.subplots(8, 8, figsize=(12, 12))
    for i, ax in enumerate(axes.flat):
        ax.imshow(filters[i, 0].numpy(), cmap='gray')  # show first input channel
        ax.axis('off')
    plt.suptitle(f'conv2 filters — {title}')
    plt.tight_layout()
    plt.savefig(f'outputs/filters_conv2_{title}.png')
    plt.show()


def plot_activations(model, n=8):
    img_batch = next(iter(test_loader))[0][:n]  # (n, 1, 28, 28)

    with torch.no_grad():
        conv1_out = model.conv1(img_batch)        # (n, 32, 28, 28)
        conv2_out = model.conv2(conv1_out)         # (n, 64, 28, 28)

    # Step 0: raw input images
    plt.figure(figsize=(n * 1.5, 2))
    for i in range(n):
        plt.subplot(1, n, i + 1)
        plt.imshow(img_batch[i, 0].numpy(), cmap='gray')
        plt.axis('off')
    plt.suptitle('Input (unfiltered)')
    plt.tight_layout()
    plt.savefig('outputs/step0_input.png')
    plt.show()

    # Step 1: after conv1 — show filter 0 response for each image
    plt.figure(figsize=(n * 1.5, 2))
    for i in range(n):
        plt.subplot(1, n, i + 1)
        plt.imshow(conv1_out[i, 0].numpy(), cmap='gray')
        plt.axis('off')
    plt.suptitle('After conv1 (filter 0)')
    plt.tight_layout()
    plt.savefig('outputs/step1_conv1.png')
    plt.show()

    # Step 2: after conv2 — show filter 0 response for each image
    plt.figure(figsize=(n * 1.5, 2))
    for i in range(n):
        plt.subplot(1, n, i + 1)
        plt.imshow(conv2_out[i, 0].numpy(), cmap='gray')
        plt.axis('off')
    plt.suptitle('After conv2 (filter 0)')
    plt.tight_layout()
    plt.savefig('outputs/step2_conv2.png')
    plt.show()


def plot_misclassified(model, n=16):
    wrong_images, wrong_labels, wrong_preds = [], [], []
    model.eval()
    with torch.no_grad():
        for x_test, y_test in test_loader:
            y_hat = model(x_test)
            preds = torch.argmax(y_hat, dim=-1)
            mask = preds != y_test
            wrong_images.append(x_test[mask])
            wrong_labels.append(y_test[mask])
            wrong_preds.append(preds[mask])
            if sum(len(w) for w in wrong_images) >= n:
                break
    wrong_images = torch.cat(wrong_images)[:n]
    wrong_labels = torch.cat(wrong_labels)[:n]
    wrong_preds = torch.cat(wrong_preds)[:n]

    cols = 4
    rows = (n + cols - 1) // cols
    plt.figure(figsize=(cols * 2, rows * 2))
    for i in range(n):
        plt.subplot(rows, cols, i + 1)
        plt.imshow(wrong_images[i, 0].numpy(), cmap='gray')
        plt.title(f'true={wrong_labels[i].item()} pred={wrong_preds[i].item()}', fontsize=8)
        plt.axis('off')
    plt.suptitle('Misclassified examples')
    plt.tight_layout()
    plt.savefig('outputs/misclassified.png')
    plt.show()


net = MyNetwork()
device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")  # Mac
# device = torch.device("cuda" if torch.cuda.is_available() else "cpu")      # NVIDIA
# net = net.to(device)

# Show raw input images before anything else
imgs, labels = next(iter(test_loader))
plt.figure(figsize=(12, 5))
for i in range(24):
    plt.subplot(3, 8, i + 1)
    plt.imshow(imgs[i, 0].numpy(), cmap='gray')
    plt.title(str(labels[i].item()), fontsize=8)
    plt.axis('off')
plt.suptitle('Input images (unfiltered)')
plt.tight_layout()
plt.savefig('outputs/input_images.png')
plt.show()

# a. Plot filters BEFORE training
plot_conv1_filters(net, 'before_training')
plot_conv2_filters(net, 'before_training')

# learning rates
SGD_lr = 0.1
Adam_lr = 0.01
AdamW_lr = 0.001
RMSprop_lr = 0.01
Adagrad_lr = 0.01
Adadelta_lr = 0.01
NAdam_lr = 0.01
ASGD_lr = 0.01

n_epochs = 5

_avg = []
optimizer = optim.SGD(net.parameters(), lr=SGD_lr)
# optimizer = optim.Adam(net.parameters(), lr=learning_rate)
# optimizer = optim.RMSprop(net.parameters(), lr=learning_rate)
# optimizer = optim.Adagrad(net.parameters(), lr=learning_rate)
# optimizer = optim.Adadelta(net.parameters(), lr=learning_rate)
# optimizer = optim.NAdam(net.parameters(), lr=learning_rate)
# optimizer = optim.ASGD(net.parameters(), lr=learning_rate)
# optimizer = optim.AdamW(net.parameters(), lr=AdamW_lr, weight_decay=0.0001)


optimizer = optim.SGD(net.parameters(), lr=SGD_lr)
scheduler = StepLR(
    optimizer, step_size=1, gamma=0.9
)  # Decays LR by 0.1 every 10 epochs

L2_lambda = 0.01
criterion = nn.CrossEntropyLoss()

all_epoch_losses = []
for i in range(n_epochs):
    net.train()
    epoch_losses = []
    for batch in train_loader:
        x, y = batch
        # x, y = x.to(device), y.to(device)
        y_hat = net(x)
        loss = criterion(y_hat, y)
        loss += L2_lambda * torch.norm(net.fc2.weight,p=2)
        loss.backward()
        optimizer.step()
        optimizer.zero_grad()
        epoch_losses.append(loss)
    avg_loss = sum(epoch_losses) / len(epoch_losses)
    all_epoch_losses.append(avg_loss)
    scheduler.step()

    print(f"Epoch {i+1} avg loss: {avg_loss.item()}")

    # Evaluate on test set and print accuracy for each epoch
    net.eval()
    correct = 0
    total = 0
    with torch.no_grad():
        for x_test, y_test in test_loader:
            # x_test, y_test = x_test.to(device), y_test.to(device)
            y_hat = net(x_test)
            preds = torch.argmax(y_hat, dim=-1)
            correct += (preds == y_test).sum().item()
            total += y_test.size(0)
    acc = correct / total
    print(f"Epoch {i+1} test accuracy: {acc * 100:.2f}%")
    _avg.append(acc)
    if break_on_first_solution and (acc - 1.0) == 0:
        break

# a. Plot filters AFTER training
plot_conv1_filters(net, 'after_training')
plot_conv2_filters(net, 'after_training')

# b. Input → conv1 → conv2 activations for one image
plot_activations(net)

# c. Plot misclassified examples
plot_misclassified(net)
