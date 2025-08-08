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
pwd = "./DataForRF/"
# Load both files
df_encoded = pd.read_csv(pwd+'data_for_RF.csv')  # The file with encoded values
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

# Example: View profile for Axiom
print("Profile for Axiom:")
print(apt_profiles["Axiom"])

# You can now access any profile by its real name:
print("\nAvailable APT profiles:", list(apt_profiles.keys()), "length: ", len(apt_profiles))

###
#End of data structure
###


###
#Random Forest Feature Engineering Approach
###

from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import MultiLabelBinarizer
import numpy as np

# Create feature vectors for each APT
feature_types = ['techniques', 'software', 'capec', 'cwe', 'cve', 'pulse']

# Initialize binarizers for each feature type
binarizers = {ft: MultiLabelBinarizer() for ft in feature_types}

# Get all possible features for each type
all_features = {}
for ft in feature_types:
    # Collect all unique features of this type across all APTs
    features = set()
    for apt in apt_profiles.values():
        features.update(apt[ft])
    all_features[ft] = sorted(features)
    # Fit the binarizer with all possible features
    binarizers[ft].fit([all_features[ft]])

# Create feature vectors for each APT
apt_features = {}
for apt_name, profile in apt_profiles.items():
    feature_vector = []
    for ft in feature_types:
        # Transform the APT's features into binary vector
        binary_vec = binarizers[ft].transform([profile[ft]]).flatten()
        feature_vector.extend(binary_vec)
    apt_features[apt_name] = feature_vector

# Create a matrix of all APT features
apt_names = list(apt_profiles.keys())
X = np.array([apt_features[name] for name in apt_names])

# For similarity analysis, we'll treat each APT as its own class
y = np.arange(len(apt_names))  # Each APT gets its own class label

# Train a Random Forest classifier
rf = RandomForestClassifier(n_estimators=100, random_state=42)
rf.fit(X, y)

# The similarity between APTs can be derived from how often they end up in the same leaf nodes
def get_apt_similarity_matrix(rf_model, X, apt_names):
    # Apply the forest to our data to get leaf indices
    leaf_indices = []
    for tree in rf_model.estimators_:
        leaf_indices.append(tree.apply(X))
    leaf_indices = np.array(leaf_indices)  # shape: (n_trees, n_samples)
    
    # Compute similarity based on shared leaves
    similarity = np.zeros((len(apt_names), len(apt_names)))
    for i in range(len(apt_names)):
        for j in range(len(apt_names)):
            # Count how many trees put these two APTs in the same leaf
            same_leaves = np.sum(leaf_indices[:, i] == leaf_indices[:, j])
            similarity[i, j] = same_leaves / len(rf_model.estimators_)
    
    return similarity

similarity_matrix = get_apt_similarity_matrix(rf, X, apt_names)

# Create DataFrame for the similarity matrix
similarity_df = pd.DataFrame(
    similarity_matrix,
    index=[apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_names],
    columns=[apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_names]
)

print("Similarity DataFrame sample:")
print(similarity_df.iloc[:5, :5])  # Show first 5x5 entries

###
#Evidence Processing (same as before)
###
# evidence for apt29
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

# First sanitize the evidence
sanitized_evidence = sanitize_evidence(evidence, df_mapping)

# Then translate to encoded versions
encoded_evidence = translate_to_encoded(sanitized_evidence, df_mapping)

print("Sanitized Evidence:")
print({k: len(v) for k, v in sanitized_evidence.items()})

print("\nEncoded Evidence:")
print({k: len(v) for k, v in encoded_evidence.items()})
print("Sample encoded values:")
for k, v in encoded_evidence.items():
    print(f"{k}: {v[:3]}")  # Show first 3 encoded values for each type

###
#Similarity Calculation with Random Forest
###

# 1. First, implement feature weighting
feature_weights = {
    'techniques': 1.5,  # Matches profile key
    'software': 1.3,    # Matches profile key
    'capec': 1.0,       # Matches profile key
    'cwe': 1.0,         # Matches profile key
    'cve': 0.8,         # Matches profile key
    'pulse': 0.7        # Matches profile key
}

# 2. Create feature vector for the evidence
def create_evidence_vector(encoded_evidence, binarizers, all_features):
    """Create a feature vector for the evidence using the same structure as APT vectors"""
    evidence_vector = []
    
    for ft in feature_types:
        if ft in encoded_evidence:
            # Get the features that exist in our database
            valid_features = [f for f in encoded_evidence[ft] if f in all_features[ft]]
            # Transform to binary vector
            binary_vec = binarizers[ft].transform([valid_features]).flatten()
        else:
            # If no features of this type, use all zeros
            binary_vec = np.zeros(len(all_features[ft]))
        evidence_vector.extend(binary_vec)
    
    return np.array(evidence_vector)

evidence_vector = create_evidence_vector(encoded_evidence, binarizers, all_features)

# 3. Calculate similarity scores using the Random Forest
def calculate_rf_similarity(evidence_vector, rf_model, X, apt_names):
    """Calculate similarity scores using Random Forest predictions"""
    # Get leaf indices for the evidence vector
    evidence_leaves = np.array([tree.apply(evidence_vector.reshape(1, -1)) for tree in rf_model.estimators_]).flatten()
    
    # Get leaf indices for all APTs
    apt_leaves = []
    for tree in rf_model.estimators_:
        apt_leaves.append(tree.apply(X))
    apt_leaves = np.array(apt_leaves)  # shape: (n_trees, n_apts)
    
    # Calculate similarity as fraction of trees where evidence and APT are in same leaf
    similarities = np.mean(apt_leaves == evidence_leaves.reshape(-1, 1), axis=0)
    
    return similarities

similarity_scores = calculate_rf_similarity(evidence_vector, rf, X, apt_names)

# 4. Create full ranked results
full_results = []
sorted_indices = np.argsort(similarity_scores)[::-1]  # Sort in descending order

for rank, idx in enumerate(sorted_indices, 1):
    apt_name = apt_names[idx]
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
        'Rank': rank,
        'APT': apt_name_dict[apt_data['encoded_id']],
        'Score': similarity_scores[idx],
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



# Add these imports at the top of the script if not already present
import matplotlib.pyplot as plt
import seaborn as sns

# ==============================================
# SIMILARITY MATRIX VISUALIZATION
# ==============================================

# We already have the similarity_matrix from earlier:
# similarity_matrix = get_apt_similarity_matrix(rf, X, apt_names)

# Get display names for APTs
display_names = [apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_names]

# Create the heatmap visualization
plt.figure(figsize=(16, 14))
sns.set(font_scale=0.7)

ax = sns.heatmap(
    similarity_matrix,
    cmap="YlOrRd",
    xticklabels=display_names,
    yticklabels=display_names,
    annot=False,  # Don't show values to avoid clutter
    square=True
)

# Adjust layout
plt.title("APT Group Similarity Matrix (Random Forest Approach)", fontsize=14)
plt.xticks(rotation=90, fontsize=8)
plt.yticks(rotation=0, fontsize=8)
plt.tight_layout()

# Show the plot
plt.show()

# Also show a text version of the top 10x10 for verification
print("\nText version of similarity matrix (first 10x10):")
print("="*90)
print(f"{'':<20}", end="")
for name in display_names[:10]:
    print(f"{name[:8]:<8}", end="")
print("\n" + "-"*90)

for i in range(10):
    print(f"{display_names[i][:20]:<20}", end="")
    for j in range(10):
        print(f"{similarity_matrix[i,j]:.2f}  ", end="")
    print()

# ==============================================
# EVIDENCE SIMILARITY RESULTS (existing code remains unchanged)
# ==============================================

# The rest of your existing code for evidence processing and similarity calculation can remain the same
# This includes:
# - The evidence processing (sanitize_evidence and translate_to_encoded)
# - The feature weighting
# - The evidence vector creation
# - The similarity score calculation
# - The results display