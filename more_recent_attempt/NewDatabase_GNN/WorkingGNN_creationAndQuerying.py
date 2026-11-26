# import pandas as pd
# import numpy as np
# from collections import defaultdict



# pwd = "./DataForGNN/"
# # 1. Import CSV file as DataFrame
# # Replace 'your_file.csv' with your actual file path
# df = pd.read_csv(pwd+'data_for_GNN.csv')
# # df = pd.read_csv(pwd+'comparison_Encodedtest_df.csv')

# # 2. Print the first 5 rows
# print("First 5 rows:")
# print(df.head(5))
# print("\n" + "="*50 + "\n")  # Separator line

# # 3. Print 5 random rows
# print("5 random rows:")
# print(df.sample(5))
# print("\n" + "="*50 + "\n")  # Separator line

# # 4. Print the last 5 rows
# print("Last 5 rows:")
# print(df.tail(5))


###
#data structure work
###


import pandas as pd
from collections import defaultdict
pwd = "./DataForGNN/"
# Load both files
df_encoded = pd.read_csv(pwd+'data_for_GNN.csv')  # The file with encoded values
df_mapping = pd.read_csv(pwd+'comparison_Encodedtest_df.csv')  # The mapping file

# Step 1.1: Create a mapping dictionary for APT names
apt_name_mapping = df_mapping[['APTnameEncoded', 'APTnameOrigin']].drop_duplicates()
apt_name_dict = dict(zip(apt_name_mapping['APTnameEncoded'], apt_name_mapping['APTnameOrigin']))

# Step 1.2: Create comprehensive profiles using actual APT names as keys
def create_apt_profiles_with_names(df_encoded, name_mapping):
    profiles = {}
    
    for _, row in df_encoded.iterrows():
        apt_encoded = row['APTname']
        apt_name = name_mapping.get(apt_encoded, f"Unknown_APT_{apt_encoded}")
        
        # Initialize the profile if it doesn't exist
        if apt_name not in profiles:
            profiles[apt_name] = {
                'encoded_id': apt_encoded,
                'techniques': set(),
                'software': set(),
                'capec': set(),
                'cwe': set(),
                'cve': set(),
                'pulse': set()
            }
        
        # Add features to the profile
        if row['techniques_str'] != -1:
            profiles[apt_name]['techniques'].add(row['techniques_str'])
        if row['software_str'] != -1:
            profiles[apt_name]['software'].add(row['software_str'])
        if row['CAPEC_ID'] != -1:
            profiles[apt_name]['capec'].add(row['CAPEC_ID'])
        if row['CWE_ID'] != -1:
            profiles[apt_name]['cwe'].add(row['CWE_ID'])
        if row['CVE_ID'] != -1:
            profiles[apt_name]['cve'].add(row['CVE_ID'])
        if row['Pulse_ID'] != -1:
            profiles[apt_name]['pulse'].add(row['Pulse_ID'])
    
    return profiles

apt_profiles = create_apt_profiles_with_names(df_encoded, apt_name_dict)

# Convert sets to lists and add counts
for apt_name in apt_profiles:
    profile = apt_profiles[apt_name]
    for key in ['techniques', 'software', 'capec', 'cwe', 'cve', 'pulse']:
        profile[key] = list(profile[key])
        profile[f'{key}_count'] = len(profile[key])



# # You can now access any profile by its real name:
# print("\nAvailable APT profiles:", list(apt_profiles.keys()), "length: ", len(apt_profiles))

###
#End of data structure
###


###
#start of GNN building graph
###

import torch
import networkx as nx
from torch_geometric.data import Data

# Create mapping dictionaries
apt_ids = list(apt_profiles.keys())
feature_types = ['techniques', 'software', 'capec', 'cwe', 'cve', 'pulse']

# Assign unique indices
apt_to_idx = {apt: i for i, apt in enumerate(apt_ids)}
feature_to_idx = {}
current_idx = len(apt_ids)

# Build edge connections
edge_index = []
for apt, features in apt_profiles.items():
    for feature_type in feature_types:
        for feature_id in features[feature_type]:
            # Create unique feature node identifier
            feature_node = f"{feature_type}_{feature_id}"
            
            if feature_node not in feature_to_idx:
                feature_to_idx[feature_node] = current_idx
                current_idx += 1
                
            # Add bidirectional edges
            edge_index.append([apt_to_idx[apt], feature_to_idx[feature_node]])
            edge_index.append([feature_to_idx[feature_node], apt_to_idx[apt]])

edge_index = torch.tensor(edge_index, dtype=torch.long).t().contiguous()

# print("edge_index: ",edge_index, " length edge_index: ", len(edge_index))




# APT nodes: learnable embeddings
apt_x = torch.arange(len(apt_ids), dtype=torch.long)

# Feature nodes: use their raw IDs (normalized)
feature_x = []
for feature_node in feature_to_idx:
    _, feature_id = feature_node.split('_')
    feature_x.append([int(feature_id)])
    
feature_x = torch.tensor(feature_x, dtype=torch.float)
feature_x = (feature_x - feature_x.mean()) / feature_x.std()  # Normalize

# Combine all node features
#x is tensor as far as i understand
x = torch.cat([
    torch.zeros(len(apt_ids)),  # Placeholder for APT embeddings
    feature_x.squeeze()
])

print("x: ",x, " length x: ", len(x))



###
#actually doing gnn architecture
###

import torch.nn as nn
import torch.nn.functional as F
from torch_geometric.nn import GCNConv

class APTSimilarityGNN(nn.Module):
    def __init__(self, hidden_dim=64):
        super().__init__()
        self.apt_embedding = nn.Embedding(len(apt_ids), hidden_dim)
        self.conv1 = GCNConv(1, hidden_dim)
        self.conv2 = GCNConv(hidden_dim, hidden_dim)
        
    def forward(self, x, edge_index):
        num_apts = len(apt_ids)
        num_nodes = x.size(0)
        num_features = num_nodes - num_apts  # This should be 10819
        
        # 1. Verify edge indices
        print(f"Max edge index: {edge_index.max().item()}, Num nodes: {num_nodes}")
        assert edge_index.max() < num_nodes, f"Edge index {edge_index.max()} exceeds node count {num_nodes}"
        
        # 2. Create mask for edges pointing to feature nodes
        edge_mask = edge_index[1] >= num_apts
        feature_edges = edge_index[:, edge_mask]
        
        # 3. Normalize feature indices to 0-based
        feature_edges[1] -= num_apts
        assert feature_edges[1].max() < num_features, \
            f"Feature index {feature_edges[1].max()} exceeds feature count {num_features}"
        
        # 4. Process feature nodes
        feature_x = x[num_apts:].unsqueeze(-1)
        h_features = F.relu(self.conv1(feature_x, feature_edges))
        h_features = self.conv2(h_features, feature_edges)
        
        # 5. Get APT embeddings
        h_apts = self.apt_embedding(torch.arange(num_apts))
        
        # 6. Aggregate features for each APT
        # Get first edge for each APT and normalize indices
        apt_feature_indices = edge_index[1, :num_apts] - num_apts
        assert apt_feature_indices.max() < num_features, \
            f"Aggregation index {apt_feature_indices.max()} exceeds features {num_features}"
        
        return h_apts + h_features[apt_feature_indices]
    

model = APTSimilarityGNN()



apt_embeddings = model(x, edge_index)

########
#testing the stuff after
######


from sklearn.metrics.pairwise import cosine_similarity

def calculate_similarities(embeddings):
    """Calculate pairwise cosine similarities between all APTs"""
    sim_matrix = cosine_similarity(embeddings.detach().numpy())
    return sim_matrix

# Calculate similarity matrix
similarity_matrix = calculate_similarities(apt_embeddings)

# # Example: Find most similar APTs to Axiom (index 0)
# axiom_similarities = similarity_matrix[0]
# most_similar = sorted(zip(apt_ids, axiom_similarities), 
#                     key=lambda x: x[1], reverse=True)[1:6]  # Skip self-similarity



# for apt, sim in most_similar:
#     print(f"{apt_name_dict[apt_profiles[apt]['encoded_id']]}: {sim:.2%}")


####
import matplotlib.pyplot as plt
import seaborn as sns

plt.figure(figsize=(12,10))
sns.heatmap(similarity_matrix, 
            xticklabels=[apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_ids],
            yticklabels=[apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_ids])
plt.title("APT Group Similarity Matrix")
plt.xticks(rotation=90)
plt.tight_layout()
plt.show()


import pandas as pd
from sklearn.metrics.pairwise import cosine_similarity

# Create similarity matrix
similarity_matrix = cosine_similarity(apt_embeddings.detach().numpy())

# Get APT names in order (using encoded_id to name mapping)
apt_names = [apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_ids]

# Create DataFrame
similarity_df = pd.DataFrame(
    similarity_matrix,
    index=apt_names,
    columns=apt_names
)

# Add a small value to diagonal to avoid perfect 1.0 scores (optional)
# import numpy as np
# np.fill_diagonal(similarity_df.values, 0.999)  # So we can easily find max similarities

# print("Similarity DataFrame sample:")
# print(similarity_df.iloc[:5, :5])  # Show first 5x5 entries


# similarity_df.to_csv("testGNNoutput.csv", index=True)




#########
#everything above works trying specific queries now, do not change any of the code
#########




#giving evidence for APT29
# evidence = {
#     'techniques_str': ['T1001.002', 'T1003.002', 'T1003.004', 'T1003.006', 'T1005', 'T1016.001', 'T1018', 'T1021.001', 
#                     'T1021.002', 'T1021.006', 'T1021.007', 'T1027.001', 'T1027.002', 'T1027.003', 'T1027.006', 
#                     'T1036.004', 'T1036.005', 'T1037', 'T1037.004', 'T1047', 'T1048.002', 'T1053.005', 'T1057', 
#                     'T1059.001', 'T1059.003', 'T1059.005', 'T1059.006', 'T1059.009', 'T1068', 'T1069.002', 
#                     'T1070.004', 'T1070.006', 'T1070.008', 'T1071.001', 'T1074.002', 'T1078', 'T1078.002', 
#                     'T1078.003', 'T1078.004', 'T1082', 'T1083', 'T1087.002', 'T1087.004', 'T1090.001', 'T1090.002', 
#                     'T1090.003', 'T1090.004', 'T1098.001', 'T1098.002', 'T1098.003', 'T1098.005', 'T1102.002', 
#                     'T1105', 'T1110.001', 'T1110.003', 'T1114.002', 'T1133', 'T1136.003', 'T1140', 'T1190', 
#                     'T1195.002', 'T1199', 'T1203', 'T1204.001', 'T1204.002', 'T1213', 'T1213.003', 'T1218.005', 
#                     'T1218.011', 'T1482', 'T1484.002', 'T1505.003', 'T1528', 'T1539', 'T1546.003', 'T1546.008', 
#                     'T1547.001', 'T1548.002', 'T1550.001', 'T1550.003', 'T1550.004', 'T1552.004', 'T1553.002', 
#                     'T1553.005', 'T1555.003', 'T1556.007', 'T1558.003', 'T1560.001', 'T1562.001', 'T1562.002', 
#                     'T1562.004', 'T1562.008', 'T1566.001', 'T1566.002', 'T1566.003', 'T1568', 'T1573', 'T1583.001', 
#                     'T1583.006', 'T1584.001', 'T1585.001', 'T1586.002', 'T1586.003', 'T1587.001', 'T1587.003', 
#                     'T1588.002', 'T1589.001', 'T1595.002', 'T1606.001', 'T1606.002', 'T1621', 'T1649', 'T1651', 
#                     'T1665'],
#     'software_str': ['S0037', 'S0559', 'S0633', 'S0002', 'S0046', 'S0050', 'S0154', 'S0357', 'S0677', 'S0048', 
#                     'S0512', 'S0514', 'S0515', 'S0634', 'S0661', 'S0597', 'S0039', 'S0521', 'S0552', 'S0684', 
#                     'S0029', 'S0053', 'S0513', 'S0565', 'S0588', 'S0139', 'S0511', 'S0518', 'S0560', 'S0562', 
#                     'S0589', 'S1084', 'S0516', 'S0049', 'S0057', 'S0150', 'S0195', 'S0051', 'S0052', 'S0054', 
#                     'S0635', 'S0636', 'S0682', 'S0096', 'S0183', 'S0175', 'S0637', 'S0100'],
#     'CAPEC_ID': ['CAPEC-204', 'CAPEC-37', 'CAPEC-545', 'CAPEC-647', 'CAPEC-292', 'CAPEC-561', 'CAPEC-572', 'CAPEC-655', 
#                 'CAPEC-35', 'CAPEC-504', 'CAPEC-543', 'CAPEC-616', 'CAPEC-564', 'CAPEC-573', 'CAPEC-560', 'CAPEC-70', 
#                 'CAPEC-312', 'CAPEC-313', 'CAPEC-580', 'CAPEC-127', 'CAPEC-497', 'CAPEC-465', 'CAPEC-481', 'CAPEC-49', 
#                 'CAPEC-565', 'CAPEC-555', 'CAPEC-186', 'CAPEC-443', 'CAPEC-523', 'CAPEC-669', 'CAPEC-673', 'CAPEC-678', 
#                 'CAPEC-691', 'CAPEC-150', 'CAPEC-650', 'CAPEC-21', 'CAPEC-31', 'CAPEC-558', 'CAPEC-593', 'CAPEC-645', 
#                 'CAPEC-60', 'CAPEC-474', 'CAPEC-485', 'CAPEC-639', 'CAPEC-206', 'CAPEC-473', 'CAPEC-68', 'CAPEC-509', 
#                 'CAPEC-578', 'CAPEC-268', 'CAPEC-571', 'CAPEC-163', 'CAPEC-542', 'CAPEC-464'],
#     'CWE_ID': ['CWE-524', 'CWE-311', 'CWE-1258', 'CWE-226', 'CWE-312', 'CWE-318', 'CWE-1278', 'CWE-285', 'CWE-200', 
#             'CWE-522', 'CWE-294', 'CWE-521', 'CWE-94', 'CWE-272', 'CWE-59', 'CWE-282', 'CWE-270', 'CWE-1021', 
#             'CWE-284', 'CWE-307', 'CWE-798', 'CWE-204', 'CWE-208', 'CWE-425', 'CWE-288', 'CWE-732', 'CWE-276', 
#             'CWE-693', 'CWE-441', 'CWE-923', 'CWE-257', 'CWE-494', 'CWE-552', 'CWE-287', 'CWE-290', 'CWE-302', 
#             'CWE-346', 'CWE-539', 'CWE-384', 'CWE-664', 'CWE-602', 'CWE-113', 'CWE-20', 'CWE-472', 'CWE-488', 
#             'CWE-330', 'CWE-327', 'CWE-325', 'CWE-117', 'CWE-451', 'CWE-359'],
#     'CVE_ID': ['CVE-2021-44228', 'wfagaed'],  # Kept your example CVE
#     'Pulse_ID': ['676652f78bbff8f3a7dcb4c3']
#     }




#the evidence fed is for Lazarus Group
evidence = {
    'techniques_str': [
        'T0865', 'T1001.003', 'T1005', 'T1008', 'T1010', 'T1012', 'T1016', 
        'T1021.001', 'T1021.002', 'T1021.004', 'T1027.002', 'T1027.007', 
        'T1027.013', 'T1033', 'T1036.003', 'T1036.004', 'T1036.005', 
        'T1036.008', 'T1041', 'T1046', 'T1047', 'T1048.003', 'T1049', 
        'T1053.005', 'T1055.001', 'T1056.001', 'T1057', 'T1059.001', 
        'T1059.003', 'T1059.005', 'T1070', 'T1070.003', 'T1070.004', 
        'T1070.006', 'T1071.001', 'T1074.001', 'T1078', 'T1082', 'T1083', 
        'T1087.002', 'T1090.001', 'T1090.002', 'T1098', 'T1102.002', 
        'T1104', 'T1105', 'T1106', 'T1110.003', 'T1124', 'T1132.001', 
        'T1134.002', 'T1140', 'T1189', 'T1202', 'T1203', 'T1204.001', 
        'T1204.002', 'T1218', 'T1218.005', 'T1218.010', 'T1218.011', 
        'T1220', 'T1221', 'T1485', 'T1489', 'T1491.001', 'T1497.001', 
        'T1497.003', 'T1505.004', 'T1529', 'T1534', 'T1542.003', 
        'T1543.003', 'T1547.001', 'T1547.009', 'T1553.002', 'T1557.001', 
        'T1560', 'T1560.001', 'T1560.002', 'T1560.003', 'T1561.001', 
        'T1561.002', 'T1562.001', 'T1562.004', 'T1564.001', 'T1566.001', 
        'T1566.002', 'T1566.003', 'T1567.002', 'T1571', 'T1573.001', 
        'T1574.002', 'T1574.013', 'T1583.001', 'T1583.004', 'T1583.006', 
        'T1584.001', 'T1584.004', 'T1585.001', 'T1585.002', 'T1587.001', 
        'T1587.002', 'T1588.002', 'T1588.003', 'T1588.004', 'T1589.002', 
        'T1591', 'T1591.004', 'T1593.001', 'T1608.001', 'T1608.002', 
        'T1614.001', 'T1620', 'T1622', 'T1656'
    ],
    'software_str': [
        'S0181', 'S0239', 'S0245', 'S0246', 'S0586', 'S0238', 'S0498', 
        'S0520', 'S0567', 'S0665', 'S0694', 'S0376', 'S0431', 'S0180', 
        'S0241', 'S0103', 'S0271', 'S0366', 'S0678', 'S0263', 'S0347', 
        'S0497', 'S0584', 'S0593', 'S0364', 'S0174', 'S0108'
    ],
    'CAPEC_ID': [
        'CAPEC-204', 'CAPEC-37', 'CAPEC-545', 'CAPEC-647', 'CAPEC-309', 
        'CAPEC-561', 'CAPEC-577', 'CAPEC-165', 'CAPEC-504', 'CAPEC-543', 
        'CAPEC-616', 'CAPEC-300', 'CAPEC-568', 'CAPEC-573', 'CAPEC-268', 
        'CAPEC-560', 'CAPEC-70', 'CAPEC-312', 'CAPEC-313', 'CAPEC-580', 
        'CAPEC-127', 'CAPEC-497', 'CAPEC-465', 'CAPEC-565', 'CAPEC-295', 
        'CAPEC-196', 'CAPEC-698', 'CAPEC-163', 'CAPEC-552', 'CAPEC-478', 
        'CAPEC-132', 'CAPEC-206', 'CAPEC-473', 'CAPEC-68', 'CAPEC-578', 
        'CAPEC-641', 'CAPEC-640', 'CAPEC-542'
    ],
    'CWE_ID': [
        'CWE-524', 'CWE-311', 'CWE-1258', 'CWE-226', 'CWE-312', 'CWE-318', 
        'CWE-1278', 'CWE-285', 'CWE-200', 'CWE-522', 'CWE-294', 'CWE-521', 
        'CWE-1021', 'CWE-117', 'CWE-307', 'CWE-798', 'CWE-204', 'CWE-208', 
        'CWE-425', 'CWE-288', 'CWE-732', 'CWE-276', 'CWE-693', 'CWE-441', 
        'CWE-384', 'CWE-664', 'CWE-829', 'CWE-451', 'CWE-284', 'CWE-59', 
        'CWE-20', 'CWE-327', 'CWE-290', 'CWE-325', 'CWE-706', 'CWE-114'
    ],
    'CVE_ID': [],  # No CVE data provided in the original
    'Pulse_ID': []  # No Pulse data provided in the original
}



# 1. First, create mapping dictionaries from comparison_Encodedtest_df.csv
feature_mappings = {
    'techniques': set(df_mapping['techniques_Origin'].dropna().unique()),
    'software': set(df_mapping['software_Origin'].dropna().unique()),
    'capec': set(df_mapping['CAPEC_ID_Origin'].dropna().unique()),
    'cwe': set(df_mapping['CWE_ID_Origin'].dropna().unique()),
    'cve': set(df_mapping['CVE_ID_Origin'].dropna().unique()),
    'pulse': set(df_mapping['Pulse_ID_Origin'].dropna().unique())
}

def sanitize_evidence(evidence, df_mapping):
    """Keep only evidence features that exist in our database"""
    sanitized = {}
    missing = {}
    
    column_mapping = {
        'techniques_str': ('techniques_Origin', 'techniques_Encoded'),
        'software_str': ('software_Origin', 'software_Encoded'),
        'CAPEC_ID': ('CAPEC_ID_Origin', 'CAPEC_ID_Encoded'),
        'CWE_ID': ('CWE_ID_Origin', 'CWE_ID_Encoded'),
        'CVE_ID': ('CVE_ID_Origin', 'CVE_ID_Encoded'),
        'Pulse_ID': ('Pulse_ID_Origin', 'Pulse_ID_Encoded')
    }
    
    for evidence_key, (origin_col, _) in column_mapping.items():
        if evidence_key in evidence:
            # Get all valid values (excluding NaN and empty strings)
            valid_values = set(df_mapping[origin_col].dropna())
            valid_values.discard('')  # Remove empty strings if any
            
            valid = []
            invalid = []
            
            for item in evidence[evidence_key]:
                # Skip empty strings but don't count them as invalid
                if item == '':
                    continue
                if str(item) in valid_values:
                    valid.append(item)
                else:
                    invalid.append(item)
            
            if valid:
                sanitized[evidence_key] = valid
            if invalid:
                missing[evidence_key] = invalid
    
    if missing:
        print("WARNING: These features were not found in our database:")
        for feature_type, items in missing.items():
            print(f"- {feature_type}: {items[:5]}")
            if len(items) > 5:
                print(f"  (and {len(items)-5} more)")
    
    return sanitized


def translate_to_encoded(evidence, df_mapping):
    """Convert sanitized evidence to encoded IDs using the mapping dataframe"""
    encoded_evidence = {}
    
    # Mapping between evidence keys and their corresponding encoded columns AND output keys
    column_mapping = {
        'techniques_str': ('techniques_Encoded', 'techniques'),
        'software_str': ('software_Encoded', 'software'), 
        'CAPEC_ID': ('CAPEC_ID_Encoded', 'capec'),
        'CWE_ID': ('CWE_ID_Encoded', 'cwe'),
        'CVE_ID': ('CVE_ID_Encoded', 'cve'),
        'Pulse_ID': ('Pulse_ID_Encoded', 'pulse')
    }
    
    for evidence_key, (encoded_col, output_key) in column_mapping.items():
        if evidence_key in evidence:
            # Create mapping dictionary for this feature type
            origin_col = encoded_col.replace('_Encoded', '_Origin')
            mapping = dict(zip(df_mapping[origin_col], df_mapping[encoded_col]))
            
            # Translate each value to its encoded version
            encoded_values = []
            for val in evidence[evidence_key]:
                if val in mapping:
                    encoded_values.append(mapping[val])
            
            if encoded_values:
                encoded_evidence[output_key] = encoded_values
    
    return encoded_evidence



# Usage:
# print("Before sanitization:")
# print({k: len(v) for k, v in evidence.items()})

clean_evidence = sanitize_evidence(evidence, df_mapping)

# print("\nAfter sanitization:")
# print({k: len(v) for k, v in clean_evidence.items()})

# First sanitize the evidence
sanitized_evidence = sanitize_evidence(evidence, df_mapping)

# Then translate to encoded versions
encoded_evidence = translate_to_encoded(sanitized_evidence, df_mapping)

# print("Sanitized Evidence:")
# print({k: len(v) for k, v in sanitized_evidence.items()})

# print("\nEncoded Evidence:")
# print({k: len(v) for k, v in encoded_evidence.items()})
# print("Sample encoded values:")
# for k, v in encoded_evidence.items():
#     print(f"{k}: {v[:3]}")  # Show first 3 encoded values for each type



#########

# 1. First, implement feature weighting
feature_weights = {
    'techniques': 1.5,  # Matches profile key
    'software': 1.3,    # Matches profile key
    'capec': 1.0,       # Matches profile key
    'cwe': 1.0,         # Matches profile key
    'cve': 0.8,         # Matches profile key
    'pulse': 0.7        # Matches profile key
}

# 2. Modified similarity calculation with weighting
def calculate_weighted_similarity(encoded_evidence, apt_embeddings, apt_profiles):
    """Calculate weighted similarity scores for all APTs"""
    similarities = []
    
    for apt_name in apt_ids:
        apt_data = apt_profiles[apt_name]
        total_score = 0
        total_weight = 0
        
        # Calculate weighted feature matches
        for feature_type, weight in feature_weights.items():
            if feature_type in encoded_evidence and feature_type in apt_data:
                evidence_set = set(encoded_evidence[feature_type])
                profile_set = set(apt_data[feature_type])
                common = len(evidence_set & profile_set)
                
                total_score += common * weight
                total_weight += len(encoded_evidence[feature_type]) * weight
        
        # Normalized similarity score
        similarity = total_score / total_weight if total_weight > 0 else 0
        similarities.append(similarity)
    
    return torch.tensor(similarities)

# 3. Get weighted similarities
weighted_similarities = calculate_weighted_similarity(encoded_evidence, apt_embeddings, apt_profiles)

# 4. Create full ranked results
full_results = []
for idx in weighted_similarities.sort(descending=True).indices:
    apt_name = apt_ids[idx]
    apt_data = apt_profiles[apt_name]
    
    # Calculate common features
    common_features = {
        'techniques': len(set(encoded_evidence.get('techniques', [])) & set(apt_data['techniques'])),
        'software': len(set(encoded_evidence.get('software', [])) & set(apt_data['software'])),
        'capec': len(set(encoded_evidence.get('capec', [])) & set(apt_data['capec'])),
        'cwe': len(set(encoded_evidence.get('cwe', [])) & set(apt_data['cwe'])),
        'cve': len(set(encoded_evidence.get('cve', [])) & set(apt_data['cve'])),
        'pulse': len(set(encoded_evidence.get('pulse', [])) & set(apt_data['pulse']))
    }
    
    full_results.append({
        'Rank': len(full_results)+1,
        'APT': apt_name_dict[apt_data['encoded_id']],
        'Score': weighted_similarities[idx].item(),
        'Encoded_ID': apt_data['encoded_id'],
        **common_features
    })

# Convert to DataFrame
ranked_apts = pd.DataFrame(full_results)

# 5. Display all 181 APTs with formatting
pd.set_option('display.max_rows', None)
pd.set_option('display.width', 1000)

print("\nComplete APT Ranking (1-181) with Feature Matches:")
print("="*110)
print(f"{'Rank':<5} | {'APT':<30} | {'Encoded ID':<10} | {'Score':<8} | {'Techniques':<10} | {'Software':<8} | {'CAPEC':<6} | {'CWE':<4} | {'CVE':<4} | {'Pulse':<5}")
print("-"*110)

for _, row in ranked_apts.iterrows():
    # Truncate long APT names to fit column width
    apt_name = (row['APT'][:27] + '..') if len(row['APT']) > 29 else row['APT']
    print(f"{row['Rank']:<5} | {apt_name:<30} | {row['Encoded_ID']:<10} | {row['Score']:.4f} | {row['techniques']:<10} | {row['software']:<8} | {row['capec']:<6} | {row['cwe']:<4} | {row['cve']:<4} | {row['pulse']:<5}")
