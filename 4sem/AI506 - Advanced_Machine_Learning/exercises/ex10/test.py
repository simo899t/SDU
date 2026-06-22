import torch
adj = torch.tensor([[ 0, 2, 0, 1, 0],
                     [1, 0, 0, 0, 0],
                     [0, 0, 0, 0, 1],
                     [1, 0, 0, 0, 0],
                     [0, 0, 1, 0, 0]], dtype=torch.float32)  # Adjacency matrix (5 nodes)

b = torch.sqrt(adj) 

print(adj)
print(b)