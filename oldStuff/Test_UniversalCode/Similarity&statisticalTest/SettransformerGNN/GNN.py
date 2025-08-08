import pandas as pd
import matplotlib.pyplot as plt
# from pandasgui import show
import seaborn as sns
import torch
from torch_geometric.data import Data
from torch.nn import Transformer

import numpy as np

import networkx as nx

import torch
import torch.nn as nn
import torch_geometric.nn as pyg_nn
from torch_geometric.data import Data
import torch.nn.functional as F

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch_geometric.data import Data
from torch_geometric.nn import GCNConv, global_mean_pool
import networkx as nx
import pandas as pd

import random
import math

from itertools import islice

df2 = pd.read_csv('./FinalDataFrame_Apr2024.csv', low_memory=False)
# print(df2.head())  # show the first few rows
# print(df2.info())  # show data types and summary statistics
# print(df2.describe())  # show summary statistics
# print(df2)

def returnTechNsoftIDs(x):
    df = x #should be the main dataframe with everything in it
    TechNsoft_df = df[df['PageType'].str.contains('Techniques|Software', case=False, na=False)]
    TechNsoft_df = TechNsoft_df[["DataTypeID"]]
        
    TechNsoft_df = TechNsoft_df.drop_duplicates()
    TechNsoft_df = TechNsoft_df.reset_index(drop=True) # Reset the index
    index = TechNsoft_df[TechNsoft_df['DataTypeID'].str.contains('T1132.002', case=False, na=False)].index.tolist()
    # print(index[0])
    # print(len(TechNsoft_df['DataTypeID']))
    value = TechNsoft_df['DataTypeID'].iloc[index[0]]
    # print(value)
    
    return(TechNsoft_df)

def recentAPTnameList(x):
    df = x #should be the main dataframe with everything in it
    filtered_df = df[df['Update_Date'].str.contains('Data_Apr2024', case=False, na=False)]
    filtered_df = filtered_df[filtered_df['PageType'].str.contains('Groups', case=False, na=False)]
    columns_to_keep = ["Title_Name"]
    filtered_df = filtered_df[columns_to_keep]

    APTnames = filtered_df['Title_Name'].drop_duplicates()
    APTnames = APTnames.tolist()
    return(APTnames)


def returnTechAndTactic(x):
    df = x #should be the main dataframe with everything in it
    Tech_df = df[df['Update_Date'].str.contains('Data_Apr2024', case=False, na=False)]
    Tech_df = Tech_df[Tech_df['PageType'].str.contains('Techniques', case=False, na=False)]
    Tech_df = Tech_df[["Title_ID", "SumUpTable_Tactic", "SumUpTable_Tactics"]]
    
    Tech_df['Tactic_s'] = Tech_df['SumUpTable_Tactic'].fillna(Tech_df['SumUpTable_Tactics'])
    Tech_df = Tech_df.drop(columns=['SumUpTable_Tactic', 'SumUpTable_Tactics'])
    Tech_df = Tech_df.drop_duplicates()
    return(Tech_df)


def MergeDFwithTectics(x, y):
    Group_df = x
    Tech_df = y
    testdf = pd.merge(Group_df, Tech_df, left_on='Tables_ID', right_on='Title_ID', how='left')

    testdf = testdf.drop(columns=['Title_ID'])# comment to verify Tactic technique associationn
    
    # lines below are responsible to have only one technique associate to one techniques at a one time
    # example
    # 4672  ZIRCONIUM  T1547.001    Enterprise  Persistence, Privilege Escalation
    # to
    # 5368  ZIRCONIUM  T1547.001    Enterprise  Persistence
    # 5369  ZIRCONIUM  T1547.001    Enterprise  Privilege Escalation
    
    # split the Tactic_s column into a list of tactics
    testdf['Tactic_s'] = testdf['Tactic_s'].str.split(', ')
    # explode the list into separate rows
    testdf = testdf.explode('Tactic_s')
    # reset the index
    testdf = testdf.reset_index(drop=True)

        # rename the columns
    testdf = testdf.rename(columns={
        'Title_Name':'APTname',
        'Tables_ID':'Techniques_N_Software',
        'Tactic_s':'Tactic_s',
        'Tables_Domain':'Domain'
    })

    return(testdf)

Group_df = df2
# Group_df = df2[df2['Update_Date'].str.contains('Data_Apr2024', case=False, na=False)]
Group_df = Group_df[Group_df['PageType'].str.contains('Groups', case=False, na=False)]
Group_df = Group_df[Group_df['Tables_TableType'].isin(['Techniques Used', 'Software'])]
Group_df = Group_df[["Title_Name", "Tables_ID", "Tables_Domain"]]
# Group_df = Group_df.drop_duplicates()
# print(Group_df)


Tech_df = returnTechAndTactic(df2)
# print(Tech_df)


data_df = MergeDFwithTectics(Group_df, Tech_df)
# print(data_df)

APTname = recentAPTnameList(df2)
lenAPTname = len(APTname)
# print(APTname)


TechSoftID = returnTechNsoftIDs(df2)["DataTypeID"].unique()

dfBinaryMatrixtest = pd.DataFrame(index=APTname, columns=TechSoftID)
# print(dfBinaryMatrixtest)

# print(df2[df2['APTname'] == 'admin@338'])
for rowName in dfBinaryMatrixtest.index.tolist():
    # print(rowName)
    SetofData = data_df[data_df['APTname'] == rowName]
    # print(SetofData)
    
    for IDs in SetofData["Techniques_N_Software"]:
        # print(IDs)
        # break
        dfBinaryMatrixtest.loc[rowName, IDs] = 1
    # break

# dfBinaryMatrixtest.fillna(0, inplace=True)
dfBinaryMatrixtest = dfBinaryMatrixtest.fillna(0).infer_objects(copy=False)

# print(dfBinaryMatrixtest)

# G = nx.Graph()
# for i, node in enumerate(dfBinaryMatrixtest.index):
#     G.add_node(node, features=dfBinaryMatrixtest.iloc[i].values)


# assume your binary matrix is in a Pandas DataFrame called `df`

G = nx.Graph()

# add nodes from the index of the DataFrame
G.add_nodes_from(dfBinaryMatrixtest.index)

# iterate over the columns (features) and create edges between nodes that share the feature
for col in dfBinaryMatrixtest.columns:
    nodes_with_feature = dfBinaryMatrixtest.index[dfBinaryMatrixtest[col] == 1]
    for i in range(len(nodes_with_feature)):
        for j in range(i+1, len(nodes_with_feature)):
            G.add_edge(nodes_with_feature[i], nodes_with_feature[j])
    
# print("Graph information:")
# print("  Number of nodes:", G.number_of_nodes())
# print("  Number of edges:", G.number_of_edges())
# print("  Node degrees:", dict(G.degree()))


adj_matrix = np.array(nx.to_numpy_array(G))
print("Adjacency matrix:")
print(adj_matrix)
print(len(adj_matrix))


nx.draw(G, with_labels=True)
plt.show()


###############################################################


# Assuming dfBinaryMatrixtest is your binary matrix DataFrame
# Convert the DataFrame to a NumPy array
binary_matrix = dfBinaryMatrixtest.values

# Load your graph G (assuming you have a way to do this)
# Example: G = nx.Graph() (replace with your actual graph loading code)
# Now we derive edge_index from your graph structure
# Replace with your specific logic for generating edge_index from G
def create_edge_index_from_graph(G):
    # Ensure G has edges
    if G.number_of_edges() == 0:
        raise ValueError("The graph has no edges.")
    
    # Extract edges
    # print(G.nodes)
    node_to_index = {node: i for i, node in enumerate(G.nodes)}
    # print("node_to_index:", node_to_index)
    # Create a list to hold the edge indices
    edge_list = []
    # print("G.edges:", G.edges)
    # Iterate through each edge in the graph
    for u, v in G.edges:
        # Convert the nodes to their indices
        edge_list.append([node_to_index[u], node_to_index[v]])
        # print("node_to_index[u]:", node_to_index[u])
        # print("node_to_index[v]:", node_to_index[v])
    
    # Create edge_index tensor
    edge_index = torch.tensor(edge_list, dtype=torch.long).t().contiguous()
    
    return edge_index


# Create edge_index based on your graph
edge_index = create_edge_index_from_graph(G)
print(edge_index)
# Number of nodes (APTs) and features (techniques)
num_nodes = binary_matrix.shape[0]
num_features = binary_matrix.shape[1]
print(num_nodes)
print(num_features)
# Create a PyG Data object
data = Data(x=torch.tensor(binary_matrix, dtype=torch.float), edge_index=edge_index)

print(data)



###############################################################


class GNNModel(nn.Module):
    def __init__(self, num_features, hidden_dim, num_nodes):
        super(GNNModel, self).__init__()
        self.fc1 = nn.Linear(num_features, hidden_dim)  # Input layer
        self.fc2 = nn.Linear(hidden_dim, num_nodes)  # Output layer

    def forward(self, x):
        x = F.relu(self.fc1(x))  # Apply ReLU activation
        x = self.fc2(x)  # Output node index
        return x

# Initialize the model, optimizer, and loss function
hidden_dim = 16
num_nodes = data.num_nodes
model = GNNModel(num_features, hidden_dim, num_nodes)
optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
criterion = nn.CrossEntropyLoss()

# Create a tensor to store the node indices
node_indices = torch.arange(num_nodes)

# Train the model
for epoch in range(2000):  # Train for 2000 epochs
    optimizer.zero_grad()
    outputs = model(data.x)  # Input features
    loss = criterion(outputs, node_indices)  # Node indices as targets
    loss.backward()
    optimizer.step()
    if (epoch + 1) % 100 == 0:  # Print every 100 epochs
        print(f"Epoch {epoch+1}, Loss: {loss.item():.4f}")


###############################################################


# Testing loop
originaldata = data
rownames = dfBinaryMatrixtest.index.tolist()
rownameslength = len(rownames)
Results_matrix = [[0 for _ in range(rownameslength)] for _ in range(rownameslength)]


RatioDict = {}
for i in range(len(APTname)):
    query_data = dfBinaryMatrixtest.iloc[i].values
    data_tensor = torch.tensor(query_data, dtype=torch.float32).unsqueeze(0)
    # Get the model output
    model.eval()
    with torch.no_grad():
        output = model(data_tensor)  # shape: (1, num_nodes)
    
    # Get the index of the node with the highest probability
    predicted_node_index = torch.argmax(output)
    RatioDict[i] = predicted_node_index.item()
    
    # print(f'Node {APTname[i]}, Predicted node index: {APTname[predicted_node_index.item()]}')


def calculate_ratio(dict1):
    equal_pairs = sum(1 for k, v in dict1.items() if k == v)
    total_pairs = len(dict1)
    ratio = (equal_pairs / total_pairs) * 100
    return ratio

ratio = calculate_ratio(RatioDict)
print(f"Ratio of equal key-value pairs: {ratio:.2f}%")


###############################################################

originaldata = data
rownames = dfBinaryMatrixtest.index.tolist()
rownameslength = len(rownames)
Results_matrix = [[0 for _ in range(rownameslength)] for _ in range(rownameslength)]


for i in range(len(APTname)):
    query_data = dfBinaryMatrixtest.iloc[i].values
    data_tensor = torch.tensor(query_data, dtype=torch.float32).unsqueeze(0)
    
    # Get the model output
    model.eval()
    with torch.no_grad():
        output = model(data_tensor)  # shape: (1, num_nodes)
        output = torch.sigmoid(output) # sigmoid keep the result between 1 and 0 
    # Get the APTs associated with the indices
    APTs = APTname
    
    # Get the corresponding values
    values = output[0].tolist()


    
    Results_matrix[i] = values
        

# print(Results_matrix)

Result_df = pd.DataFrame(Results_matrix, index=APTname, columns=APTname)
print(Result_df)  

###############################################################

# Result_df.to_csv('/home/morzen/Desktop/aGNNresultsexcel.csv', 
#           index=True,       # Don't include the row index
#           header=True,       # Include the column names
#           sep=',',           # Delimiter (default is comma)
#           encoding='utf-8')  # E

###############################################################

# TEST Entering Technique And software found
# SINGLE QUERIES


#EXPECTED Lazarus Group from this repport: https://securelist.com/unveiling-lazarus-new-campaign/110888/
# ListOfID_TechNsoft = ["T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"]
#EXPECTED andariel from this repport: https://securelist.com/andariel-evolves-to-target-south-korea-with-ransomware/102811/
# ListOfID_TechNsoft = ["T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"]
#EXPECTED Sandworm from this repport: https://cloud.google.com/blog/topics/threat-intelligence/sandworm-disrupts-power-ukraine-operational-technology/
# ListOfID_TechNsoft = ["T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"]
# expected admin@338
# ListOfID_TechNsoft = ['T1016', 'T1069.001', 'S0039', 'T1203', 'S0043', 'T1036.005', 'S0042', 'S0104', 'T1566.001', 'S0100', 'T1049', 'S0096', 'S0012', 'T1083', 'T1007', 'T1087.001', 'T1059.003', 'T1082', 'T1204.002']
# expected apt3
ListOfID_TechNsoft = ['T1095', 'T1566.002', 'T1087.001', 'S0165', 'T1564.003', 'T1546.008', 'T1090.002', 'T1027.002', 'T1218.011', 'T1098', 'T1016', 'T1005', 'T1070.004', 'T1033', 'T1021.002', 'T1069', 'T1021.001', 'T1204.001', 'T1105', 'T1027.005', 'T1110.002', 'T1056.001', 'T1041', 'T1059.003', 'T1555.003', 'T1547.001', 'T1560.001', 'T1059.001', 'T1018', 'S0349', 'T1057', 'T1049', 'S0063', 'T1082', 'T1074.001', 'T1543.003', 'T1136.001', 'T1203', 'T1104', 'S0013', 'T1027', 'T1053.005', 'S0166', 'T1078.002', 'T1003.001', 'T1574.002', 'T1083', 'S0111', 'T1552.001']


# Remove NaN values using pandas
ListOfID_TechNsoft = pd.Series(ListOfID_TechNsoft).dropna().tolist()

# Assuming ListOfID_TechNsoft and dfBinaryMatrixtest are already defined as per your context
ListOfID_TechNsoft = list(set(ListOfID_TechNsoft))
missing_columns = list(set(ListOfID_TechNsoft) - set(dfBinaryMatrixtest.columns))

if len(missing_columns) > 0:
    print(missing_columns)
    print("are not existing features")

costumDF = pd.DataFrame(0, index=[0], columns=dfBinaryMatrixtest.columns)
costumDF.loc[0, ListOfID_TechNsoft] = 1

query_data = costumDF.iloc[0].values

# FOR DIRECT INPUT FROM DATASET
# uncomment the following line i correspond to full APT profile 0-151 for now
# query_data = dfBinaryMatrixtest.iloc[i].values

data_tensor = torch.tensor(query_data, dtype=torch.float32).unsqueeze(0)

# Get the model output
model.eval()
with torch.no_grad():
    output = model(data_tensor)

# Get the indices of the tensor output
indices = torch.argsort(output, descending=True)

# Get the APTs associated with the indices
APTs = [APTname[i.item()] for i in indices[0]]

# Get the corresponding values
values = [output[0, i.item()].item() for i in indices[0]]

# Create a dictionary to store the results
results_dict = {APTs[i]: values[i] for i in range(len(APTs))}

# Print the results in the format you want
for apt, value in results_dict.items():
    print(f'{apt}: {value}')

# Optionally, return the dictionary as well
# print(results_dict)


###############################################################

# SHUFFLE TTPs OF APTs AND SUB-DIVID THEM TO KEEP LOWER SET
def shuffle_and_divide(original_list, divisor):
    # Shuffle the list
    random.shuffle(original_list)
    
    # Calculate the number of items to keep by rounding up
    items_to_keep = math.ceil(len(original_list) / divisor)
    
    # Return the remaining items in a new list
    return original_list[:items_to_keep]
    
#DIVIDING NUMBER 
SubsetDivisionSize = 1

keys = APTname
TTP_dict = dict.fromkeys(keys)

for key in TTP_dict:
    TTP_list = Group_df[Group_df['Title_Name'] == key]['Tables_ID'].drop_duplicates().tolist()

    result = shuffle_and_divide(TTP_list, SubsetDivisionSize)
    
    TTP_dict[key] = result
    


    
# print(TTP_dict["APT3"])

###############################################################

# the expected results is the key, the evidence the item

# LINE BELOW IS FOR COSTUM SETS OF APTs TTPs TO ANALYSE
# DictOFlist = {
#     "Lazarus Group":["T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"],
#     "andariel":["T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"],
#     "Sandworm":["T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"]
# }

# REMOVE CAMPAIN FROM FEATURE KEEP ONLY TECHNIQUE AND SOFTWARE CHECK IF SAME ISSUE ON SIAMESE NETWORK

# THE LINE BELOW IS FOR SUBSETS OF ALL APTs
# DictOFlist = TTP_dict

# IMPORTED USE CASE DATA SET
# Step 1: Read the CSV file into a DataFrame
df = pd.read_csv('/home/morzen/Desktop/CTI_usecase.csv')

# Step 2: Convert the DataFrame to a dictionary with lists
result_dict_USECASE = df.groupby('Names')['ListTechNSoft'].apply(list).to_dict()

DictOFlist = result_dict_USECASE



resultdict = {}

for element in DictOFlist:
    # print(element)
    # print(DictOFlist[element])
    ListOfID_TechNsoft = DictOFlist[element]

    # Remove NaN values using pandas
    ListOfID_TechNsoft = pd.Series(ListOfID_TechNsoft).dropna().tolist()

    ListOfID_TechNsoft = list(set(ListOfID_TechNsoft))
    missing_columns = list(set(ListOfID_TechNsoft) - set(dfBinaryMatrixtest.columns))
    
    if len(missing_columns) > 0:
        print(missing_columns)
        ListOfID_TechNsoft = [item for item in ListOfID_TechNsoft if item not in missing_columns]
        print("are not existing features")
    
    costumDF = pd.DataFrame(0, index=[0], columns=dfBinaryMatrixtest.columns)
    costumDF.loc[0, ListOfID_TechNsoft] = 1
    
    query_data = costumDF.iloc[0].values
    
    # For direct input from dataset, uncomment the following line
    # query_data = dfBinaryMatrixtest.iloc[i].values
    
    data_tensor = torch.tensor(query_data, dtype=torch.float32).unsqueeze(0)
    
    # Get the model output
    model.eval()
    with torch.no_grad():
        output = model(data_tensor)
    
    # Get the indices of the tensor output
    indices = torch.argsort(output, descending=True)
    
    # Get the APTs associated with the indices
    APTs = [APTname[i.item()] for i in indices[0]]
    
    # Get the corresponding values
    values = [output[0, i.item()].item() for i in indices[0]]
    
    # Create a dictionary to store the results
    results_dict = {APTs[i]: values[i] for i in range(len(APTs))}
    
    # Print the results in the format you want
    # for apt, value in results_dict.items():
    #     print(f'{apt}: {value}')

    resultdict[element] = results_dict
    
    # print(list(resultdict[element].items())[0])
    # print("\n")
    
    # for key, value in resultdict[element].items():
    #     print(f"{key}: {value}")
    
    

###############################################################

# RATIO OF SUCESS FAILURE ANALYSIS

# LINE BELOW LOOK AT THE TOPx FOR SUCESS
topX = 5

lengthResultDict = len(resultdict)
SucessFailDict = dict()

sucess = 0
failure = 0
# Iterate through the dictionary
for key, value in resultdict.items():
    # Check if the value is a list
    if isinstance(value, dict):
        # Slice the list to keep only the first topX items
        Sliced_Dict = {}
        Sliced_Dict[key] = dict(islice(value.items(), topX))

        # print(len(value))
        # print(len(Sliced_Dict[key]))

        if key in Sliced_Dict[key]:
            sucess = sucess + 1
            SucessFailDict[key]= "Pass"
        else:
            failure = failure + 1
            SucessFailDict[key]= "Fail"

    else:
        print("didn't divide the list, testing failed")


print("settings: "+"Topx: "+str(topX)+" sets Divided by: "+str(SubsetDivisionSize))
print("GNN for this set had a success failure rate of \n"+str(sucess)+"/"+str(failure)+" Total: "+ str(lengthResultDict))
print("in percentage sucess rate: "+str((sucess/lengthResultDict)*100))

###############################################################

# GIVE PASS FAIL FOR EACH APT GIVEN

for element in SucessFailDict:
    print(element + " : " + SucessFailDict[element])