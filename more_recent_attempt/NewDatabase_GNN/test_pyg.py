# import torch
# from torch_geometric import __version__

# print(f"PyTorch: {torch.__version__}")
# print(f"PyG: {__version__}")
# print(f"MPS available: {torch.backends.mps.is_available()}")
# print(f"Device: {torch.device('mps' if torch.backends.mps.is_available() else 'cpu')}")



import pandas as pd
import numpy as np
from collections import defaultdict



pwd = "./DataForGNN/"
# 1. Import CSV file as DataFrame
# Replace 'your_file.csv' with your actual file path
# df = pd.read_csv(pwd+'data_for_GNN.csv')
df = pd.read_csv(pwd+'comparison_Encodedtest_df.csv')

# 2. Print the first 5 rows
print("First 5 rows:")
print(df.head(5))
print("\n" + "="*50 + "\n")  # Separator line

# 3. Print 5 random rows
print("5 random rows:")
print(df.sample(5))
print("\n" + "="*50 + "\n")  # Separator line

# 4. Print the last 5 rows
print("Last 5 rows:")
print(df.tail(5))
