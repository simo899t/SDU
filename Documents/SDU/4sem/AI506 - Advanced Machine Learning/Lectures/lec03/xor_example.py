
import torch
import torch.nn as nn
from torch.optim import AdamW
from torch.utils.data import DataLoader
import matplotlib.pyplot as plt



import random
import numpy as np
import torch

def main():
    # Set seed for reproducibility
    seed = 42
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

    # Hyperparameters
    n_epochs = 100
    bsz = 100
    break_on_first_solution = False
    lr = 0.001
    n_hidden = 5

    # Dataset size
    n = 10000
    n_train = 5000
    n_test = n - n_train

    # Build true 2D XOR dataset
    x = torch.rand(n, 2) < 0.5
    y = torch.logical_xor(x[:, 0], x[:, 1]).long()
    x = x.float()

    print("Y mean (should be about 0.5):", y.float().mean())

    # split..
    x_train, y_train = x[:n_train], y[:n_train]
    x_test, y_test = x[n_train:], y[n_train:]


    ##############################################
    # Switch between Variant 1 and Variant 2 here,
    # to see the effect of ReLU.

    # Variant 1: A linear model with two layers.
    # model = nn.Sequential(nn.Linear(2, n_hidden), nn.Linear(n_hidden, 2))

    # Variant 2: A multi-layer perceptron with two layers and a ReLU activation function.
    model = nn.Sequential(nn.Linear(2, n_hidden), nn.ReLU(), nn.Linear(n_hidden, 2))

    ##############################################

    optimizer = AdamW(model.parameters(), lr=lr)
    criterion = nn.CrossEntropyLoss()

    train_loader = DataLoader(list(zip(x_train, y_train)), shuffle=True, batch_size=bsz)


    for i in range(n_epochs):
        model.train()
        for batch in train_loader:
            optimizer.zero_grad()
            x, y = batch
            y_hat = model(x)
            loss = criterion(y_hat, y)
            loss.backward()
            optimizer.step()
            print(f"Epoch {i} Loss:", loss.item())

        # evalye
        model.eval()
        y_hat = model(x_test)
        acc = (torch.argmax(y_hat, dim=-1) == y_test).sum().float() / y_test.size(0)
        print(f"Epoch {i} Accuracy: {acc.item()}")
        if break_on_first_solution and (acc - 1.0).abs() < 1e-5:
            break

    # Visualization after training: Only show decision boundary heatmap
    import matplotlib.patches as mpatches
    plt.figure(figsize=(6, 5))
    plt.title("Model decision boundary (XOR)")
    xx, yy = torch.meshgrid(torch.linspace(0, 1, 100), torch.linspace(0, 1, 100), indexing='ij')
    grid = torch.stack([xx.flatten(), yy.flatten()], dim=1)
    with torch.no_grad():
        zz = torch.argmax(model(grid), dim=-1).reshape(xx.shape)
    contour = plt.contourf(xx, yy, zz, levels=1, alpha=0.3, cmap="coolwarm")
    plt.xlabel("x0")
    plt.ylabel("x1")
    # Add legend for class colors
    red_patch = mpatches.Patch(color=contour.cmap(1.0), label='Class 1 (XOR=1)')
    blue_patch = mpatches.Patch(color=contour.cmap(0.0), label='Class 0 (XOR=0)')
    plt.legend(handles=[blue_patch, red_patch], loc='upper right')
    plt.tight_layout()
    plt.show()


if __name__ == "__main__":
    main()
