import pandas as pd
import matplotlib.pyplot as plt
# from pandasgui import show
import seaborn as sns
import torch
from torch_geometric.data import Data
from torch.nn import Transformer
from torch.utils.data import Dataset, DataLoader
import torch.optim as optim
import numpy as np

import networkx as nx

import torch
import torch.nn as nn
import torch_geometric.nn as pyg_nn
from torch_geometric.data import Data

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
print(data_df)

APTname = recentAPTnameList(df2)
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
print(dfBinaryMatrixtest)

###############################################################

# Define the dataset to handle pairs of rows from the DataFrame
class SiameseDataset(Dataset):
    def __init__(self, dataframe, labels):
        self.dataframe = dataframe
        self.labels = labels
        self.pairs = self._create_pairs()

    def _create_pairs(self):
        # Create all possible pairs from the DataFrame rows
        pairs = []
        labels = []
        for i in range(len(self.dataframe)):
            for j in range(i + 1, len(self.dataframe)):
                pairs.append((self.dataframe.iloc[i].values, self.dataframe.iloc[j].values))
                labels.append(self.labels[i])  # Define label (APT name) for now using dummy values
        return list(zip(pairs, labels))

    def __len__(self):
        return len(self.pairs)

    def __getitem__(self, idx):
        (row1, row2), label = self.pairs[idx]
        return torch.tensor(row1, dtype=torch.float32), torch.tensor(row2, dtype=torch.float32), torch.tensor(label, dtype=torch.long)

# Siamese Network Model
class SiameseNetwork(nn.Module):
    def __init__(self, input_dim, num_nodes):
        super(SiameseNetwork, self).__init__()
        self.fc1 = nn.Linear(input_dim, 128)
        self.fc2 = nn.Linear(128, 64)
        self.fc3 = nn.Linear(64, num_nodes)  # Output layer with num_nodes outputs

    def forward_one(self, x):
        x = torch.relu(self.fc1(x))
        x = torch.relu(self.fc2(x))
        x = self.fc3(x)
        return x

    def forward(self, input1, input2):
        output1 = self.forward_one(input1)
        output2 = self.forward_one(input2)
        return output1, output2

# Loss function for Siamese Network
class CrossEntropyLoss(nn.Module):
    def __init__(self):
        super(CrossEntropyLoss, self).__init__()
        self.loss_fn = nn.CrossEntropyLoss()

    def forward(self, output, label):
        return self.loss_fn(output, label)

# Training function
def train_siamese_network(df, labels, num_nodes):
    input_dim = df.shape[1]  # Number of features in the DataFrame
    siamese_dataset = SiameseDataset(df, labels)  # Create the dataset from DataFrame
    dataloader = DataLoader(siamese_dataset, batch_size=16, shuffle=True)  # DataLoader for batch processing

    model = SiameseNetwork(input_dim, num_nodes)  # Initialize the model
    criterion = CrossEntropyLoss()  # Loss function
    optimizer = optim.Adam(model.parameters(), lr=0.001)  # Optimizer

    num_epochs = 7  # Number of training epochs
    for epoch in range(num_epochs):
        total_loss = 0
        for data1, data2, label in dataloader:
            optimizer.zero_grad()
            output1, output2 = model(data1, data2)  # Forward pass
            loss = criterion(output1, label)  # Compute loss
            loss.backward()  # Backpropagation
            optimizer.step()  # Update weights
            total_loss += loss.item()

        print(f"Epoch [{epoch+1}/{num_epochs}], Loss: {total_loss/len(dataloader)}")

    return model



rownames = dfBinaryMatrixtest.index.tolist()
labels = [rownames.index(i) for i in rownames]
trained_model = train_siamese_network(dfBinaryMatrixtest, labels, len(rownames))

###############################################################

# Function to query the Siamese network
def query_siamese_network(model, data):
    # Convert data into tensors
    data_tensor = torch.tensor(data, dtype=torch.float32).unsqueeze(0)  # Add batch dimension

    # Forward pass through the model to get output probabilities
    with torch.no_grad():  # Disable gradient calculation for inference
        output = model.forward_one(data_tensor)

        

    return output

###############################################################

rownames = dfBinaryMatrixtest.index.tolist()
rownameslength = len(rownames)
Results_matrix = [[0 for _ in range(rownameslength)] for _ in range(rownameslength)]


for i in range(len(APTname)):
    query_data = dfBinaryMatrixtest.iloc[i].values
    output = query_siamese_network(trained_model, query_data)
    # print(output)

    # probabilities = torch.sigmoid(output) # sigmoid keep the result between 1 and 0
    probabilities = torch.softmax(output, dim=1) # softmax will also keep the result between 1 and 0
    # print("probabilities")
    # print(probabilities)

    values = probabilities[0].tolist()
    Results_matrix[i] = values
        

# print(Results_matrix)

Result_df = pd.DataFrame(Results_matrix, index=APTname, columns=APTname)
print(Result_df) 

###############################################################

# Result_df.to_csv('/home/morzen/Desktop/softmax_SiamsesNetworkResultsHeatMapFOR_Mphil_PhD_transfer.csv', 
#           index=True,       # Don't include the row index
#           header=True,       # Include the column names
#           sep=',',           # Delimiter (default is comma)
#           encoding='utf-8')  # E

###############################################################

# input custom chosen features
#EXPECTED Lazarus Group from this repport: https://securelist.com/unveiling-lazarus-new-campaign/110888/
# ListOfID_TechNsoft = ["T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"]
#EXPECTED andariel from this repport: https://securelist.com/andariel-evolves-to-target-south-korea-with-ransomware/102811/
# ListOfID_TechNsoft = ["T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"]
#EXPECTED Sandworm from this repport: https://cloud.google.com/blog/topics/threat-intelligence/sandworm-disrupts-power-ukraine-operational-technology/
ListOfID_TechNsoft = ["T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"]

# Remove NaN values using pandas
ListOfID_TechNsoft = pd.Series(ListOfID_TechNsoft).dropna().tolist()

ListOfID_TechNsoft = list(set(ListOfID_TechNsoft))
missing_columns = list(set(ListOfID_TechNsoft) - set(dfBinaryMatrixtest.columns))

if len(missing_columns) >0:
    print(missing_columns)
    print("are not existing features")


costumDF = pd.DataFrame(0, index=[0], columns=dfBinaryMatrixtest.columns) 
costumDF.loc[0, ListOfID_TechNsoft] = 1
print(costumDF)

# input any full features from any node
query_data=costumDF.iloc[0].values

# query_data = dfBinaryMatrixtest.iloc[0].values


output = query_siamese_network(trained_model, query_data)
# print(output)

probabilities = torch.softmax(output, dim=1)
# print("probabilities")
# print(probabilities)
_, predicted_node = torch.max(probabilities, dim=1)

# Get the indices of the sorted probabilities
sorted_indices = torch.argsort(probabilities, dim=1, descending=True)

# Get the corresponding node names
sorted_node_names = [rownames[i.item()] for i in sorted_indices[0]]

# Get the corresponding probabilities
sorted_probabilities = [probabilities[0, i.item()].item() for i in sorted_indices[0]]

# Create a dictionary to store the node names and their probabilities
node_scores = {node_name: probability for node_name, probability in zip(sorted_node_names, sorted_probabilities)}

# Print the dictionary
# print(node_scores)

for key, value in node_scores.items():
    print(f"{key}: {value}")

###############################################################

import random
import math

def shuffle_and_divide(original_list, divisor):
    # Shuffle the list
    random.shuffle(original_list)
    
    # Calculate the number of items to keep by rounding up
    items_to_keep = math.ceil(len(original_list) / divisor)
    
    # Return the remaining items in a new list
    return original_list[:items_to_keep]
    
    
SubsetDivisionSize = 1

keys = APTname
TTP_dict = dict.fromkeys(keys)

for key in TTP_dict:
    TTP_list = Group_df[Group_df['Title_Name'] == key]['Tables_ID'].drop_duplicates().tolist()

    result = shuffle_and_divide(TTP_list, SubsetDivisionSize)
    
    TTP_dict[key] = result
    


    
# print(TTP_dict["ZIRCONIUM"])

###############################################################


# input custom chosen features
#EXPECTED Lazarus Group from this repport: https://securelist.com/unveiling-lazarus-new-campaign/110888/
# ListOfID_TechNsoft = ["T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"]
#EXPECTED andariel from this repport: https://securelist.com/andariel-evolves-to-target-south-korea-with-ransomware/102811/
# ListOfID_TechNsoft = ["T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"]
#EXPECTED Sandworm from this repport: https://cloud.google.com/blog/topics/threat-intelligence/sandworm-disrupts-power-ukraine-operational-technology/
# ListOfID_TechNsoft = ["T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"]


# the expected results is the key, the evidence the item
DictOFlist = {
    "Lazarus Group":["T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"],
    "andariel":["T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"],
    "Sandworm":["T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"]
}

# DictOFlist = TTP_dict
# print(DictOFlist)

# IMPORTED USE CASE DATA SET
df = pd.read_csv('/home/morzen/Desktop/CTI_usecase.csv')

result_dict_USECASE = df.groupby('Names')['ListTechNSoft'].apply(list).to_dict()

DictOFlist = result_dict_USECASE



resultdict = {}


for element in DictOFlist:
    # print(element)
    ListOfID_TechNsoft = DictOFlist[element]

    # Remove NaN values using pandas
    ListOfID_TechNsoft = pd.Series(ListOfID_TechNsoft).dropna().tolist()

    ListOfID_TechNsoft = list(set(ListOfID_TechNsoft))
    missing_columns = list(set(ListOfID_TechNsoft) - set(dfBinaryMatrixtest.columns))
    
    if len(missing_columns) >0:
        print(missing_columns)
        ListOfID_TechNsoft = [item for item in ListOfID_TechNsoft if item not in missing_columns]
        print("are not existing features")
    
    # make a one row df with the column for every TTP
    costumDF = pd.DataFrame(0, index=[0], columns=dfBinaryMatrixtest.columns) 
    # make the costumDF which is full of 0 so far, update and have 1s at the palce given by ListOfID_TechNsoft
    costumDF.loc[0, ListOfID_TechNsoft] = 1
    # input any full features from any node
    query_data=costumDF.iloc[0].values
    
    # query_data = dfBinaryMatrixtest.iloc[0].values
    
    
    output = query_siamese_network(trained_model, query_data)
    # print(output)
    
    probabilities = torch.softmax(output, dim=1)
    # print("probabilities")
    # print(probabilities)
    _, predicted_node = torch.max(probabilities, dim=1)
    
    # Get the indices of the sorted probabilities
    sorted_indices = torch.argsort(probabilities, dim=1, descending=True)
    
    # Get the corresponding node names
    sorted_node_names = [rownames[i.item()] for i in sorted_indices[0]]
    
    # Get the corresponding probabilities
    sorted_probabilities = [probabilities[0, i.item()].item() for i in sorted_indices[0]]
    
    # Create a dictionary to store the node names and their probabilities
    node_scores = {node_name: probability for node_name, probability in zip(sorted_node_names, sorted_probabilities)}
    
    # Print the dictionary
    # print(node_scores)
    
    # for key, value in node_scores.items():
    #     print(f"{key}: {value}")

    resultdict[element] = node_scores
    

###############################################################


# input custom chosen features
#EXPECTED Lazarus Group from this repport: https://securelist.com/unveiling-lazarus-new-campaign/110888/
# ListOfID_TechNsoft = ["T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"]
#EXPECTED andariel from this repport: https://securelist.com/andariel-evolves-to-target-south-korea-with-ransomware/102811/
# ListOfID_TechNsoft = ["T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"]
#EXPECTED Sandworm from this repport: https://cloud.google.com/blog/topics/threat-intelligence/sandworm-disrupts-power-ukraine-operational-technology/
# ListOfID_TechNsoft = ["T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"]


# the expected results is the key, the evidence the item
DictOFlist = {
    "Lazarus Group":["T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"],
    "andariel":["T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"],
    "Sandworm":["T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"]
}

# DictOFlist = TTP_dict
# print(DictOFlist)

# IMPORTED USE CASE DATA SET
df = pd.read_csv('/home/morzen/Desktop/CTI_usecase.csv')

result_dict_USECASE = df.groupby('Names')['ListTechNSoft'].apply(list).to_dict()

DictOFlist = result_dict_USECASE



resultdict = {}


for element in DictOFlist:
    # print(element)
    ListOfID_TechNsoft = DictOFlist[element]

    # Remove NaN values using pandas
    ListOfID_TechNsoft = pd.Series(ListOfID_TechNsoft).dropna().tolist()

    ListOfID_TechNsoft = list(set(ListOfID_TechNsoft))
    missing_columns = list(set(ListOfID_TechNsoft) - set(dfBinaryMatrixtest.columns))
    
    if len(missing_columns) >0:
        print(missing_columns)
        ListOfID_TechNsoft = [item for item in ListOfID_TechNsoft if item not in missing_columns]
        print("are not existing features")
    
    # make a one row df with the column for every TTP
    costumDF = pd.DataFrame(0, index=[0], columns=dfBinaryMatrixtest.columns) 
    # make the costumDF which is full of 0 so far, update and have 1s at the palce given by ListOfID_TechNsoft
    costumDF.loc[0, ListOfID_TechNsoft] = 1
    # input any full features from any node
    query_data=costumDF.iloc[0].values
    
    # query_data = dfBinaryMatrixtest.iloc[0].values
    
    
    output = query_siamese_network(trained_model, query_data)
    # print(output)
    
    probabilities = torch.softmax(output, dim=1)
    # print("probabilities")
    # print(probabilities)
    _, predicted_node = torch.max(probabilities, dim=1)
    
    # Get the indices of the sorted probabilities
    sorted_indices = torch.argsort(probabilities, dim=1, descending=True)
    
    # Get the corresponding node names
    sorted_node_names = [rownames[i.item()] for i in sorted_indices[0]]
    
    # Get the corresponding probabilities
    sorted_probabilities = [probabilities[0, i.item()].item() for i in sorted_indices[0]]
    
    # Create a dictionary to store the node names and their probabilities
    node_scores = {node_name: probability for node_name, probability in zip(sorted_node_names, sorted_probabilities)}
    
    # Print the dictionary
    # print(node_scores)
    
    # for key, value in node_scores.items():
    #     print(f"{key}: {value}")

    resultdict[element] = node_scores
    

###############################################################

from itertools import islice

topX = 1

lengthResultDict = len(resultdict)
SucessFailDict = dict()

sucess = 0
failure = 0
# Iterate through the dictionary
for key, value in resultdict.items():
    # Check if the value is a list
    # print(key)
    # print(value)
    if isinstance(value, dict):
        # Slice the list to keep only the first 5 items
        resultdict[key] = dict(islice(value.items(), topX))


        if key in resultdict[key]:
            sucess = sucess + 1
            SucessFailDict[key]= "Pass"
        else:
            failure = failure + 1
            SucessFailDict[key]= "Fail"

    else:
        print("didn't divide the list, testing failed")

print("settings: "+"Topx: "+str(topX)+" sets Divided by: "+str(SubsetDivisionSize))
print("siamese network for this set had a success failure rate of \n"+str(sucess)+"/"+str(failure)+" Total: "+ str(lengthResultDict))
print("in percentage  sucess rate: "+str((sucess/lengthResultDict)*100))
        

###############################################################

# for key, value in resultdict["Lazarus Group"].items():
#         print(f"{key}: {value}")

for element in SucessFailDict:
    print(element + " : " + SucessFailDict[element])
