import pandas as pd
import numpy as np
from collections import defaultdict

# Load data files
pwd = "./DataForTVD/"
df_encoded = pd.read_csv(pwd+'data_for_TVD.csv')
df_mapping = pd.read_csv(pwd+'comparison_Encodedtest_df.csv')

# Create APT name mapping
apt_name_mapping = df_mapping[['APTnameEncoded', 'APTnameOrigin']].drop_duplicates()
apt_name_dict = dict(zip(apt_name_mapping['APTnameEncoded'], apt_name_mapping['APTnameOrigin']))

# Create APT profiles with actual names as keys
def create_apt_profiles_with_names(df_encoded, name_mapping):
    profiles = {}
    
    for _, row in df_encoded.iterrows():
        apt_encoded = row['APTname']
        apt_name = name_mapping.get(apt_encoded, f"Unknown_APT_{apt_encoded}")
        
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

# Evidence processing functions
def sanitize_evidence(evidence, df_mapping):
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
            valid_values = set(df_mapping[origin_col].dropna())
            valid_values.discard('')
            
            valid = []
            invalid = []
            
            for item in evidence[evidence_key]:
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
    encoded_evidence = {}
    
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
            origin_col = encoded_col.replace('_Encoded', '_Origin')
            mapping = dict(zip(df_mapping[origin_col], df_mapping[encoded_col]))
            
            encoded_values = []
            for val in evidence[evidence_key]:
                if val in mapping:
                    encoded_values.append(mapping[val])
            
            if encoded_values:
                encoded_evidence[output_key] = encoded_values
    
    return encoded_evidence

# Total Variation Distance implementation
def calculate_tvd_similarity(encoded_evidence, apt_profiles, feature_weights):
    """Calculate similarity using Total Variation Distance with feature weighting"""
    similarities = []
    
    # First get all possible features to create probability distributions
    all_features = {
        'techniques': set(),
        'software': set(),
        'capec': set(),
        'cwe': set(),
        'cve': set(),
        'pulse': set()
    }
    
    # Collect all unique features across all APTs
    for apt_name in apt_profiles:
        for feature_type in all_features:
            all_features[feature_type].update(apt_profiles[apt_name][feature_type])
    
    # Convert to lists for indexing
    feature_indices = {ft: list(features) for ft, features in all_features.items()}
    
    for apt_name in apt_profiles:
        apt_data = apt_profiles[apt_name]
        total_similarity = 0
        total_weight = 0
        
        for feature_type, weight in feature_weights.items():
            # Skip if evidence doesn't have this feature type
            if feature_type not in encoded_evidence:
                continue
                
            # Create probability distributions
            evidence_features = set(encoded_evidence[feature_type])
            profile_features = set(apt_data[feature_type])
            all_possible = feature_indices[feature_type]
            
            # Create binary vectors (1 if feature present, 0 otherwise)
            evidence_vec = np.array([1 if f in evidence_features else 0 for f in all_possible])
            profile_vec = np.array([1 if f in profile_features else 0 for f in all_possible])
            
            # Normalize to probability distributions
            evidence_dist = evidence_vec / evidence_vec.sum() if evidence_vec.sum() > 0 else evidence_vec
            profile_dist = profile_vec / profile_vec.sum() if profile_vec.sum() > 0 else profile_vec
            
            # Calculate TVD (half the L1 distance between distributions)
            tvd = 0.5 * np.sum(np.abs(evidence_dist - profile_dist))
            
            # Convert to similarity (1 - TVD)
            feature_similarity = (1 - tvd) * weight
            total_similarity += feature_similarity
            total_weight += weight
        
        # Normalize by total weight
        normalized_similarity = total_similarity / total_weight if total_weight > 0 else 0
        similarities.append(normalized_similarity)
    
    return similarities

# Feature weights for TVD calculation
feature_weights = {
    'techniques': 1.5,
    'software': 1.3,
    'capec': 1.0,
    'cwe': 1.0,
    'cve': 0.8,
    'pulse': 0.7
}

# Example evidence (for Lazarus Group)
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
#     'CVE_ID': ['CVE-2021-44228', 'wfagaed'],
#     'Pulse_ID': ['676652f78bbff8f3a7dcb4c3']
# }

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

# Process evidence
clean_evidence = sanitize_evidence(evidence, df_mapping)
encoded_evidence = translate_to_encoded(clean_evidence, df_mapping)

# Calculate similarities using TVD
tvd_similarities = calculate_tvd_similarity(encoded_evidence, apt_profiles, feature_weights)

# Create ranked results
apt_ids = list(apt_profiles.keys())
ranked_results = sorted(zip(apt_ids, tvd_similarities), key=lambda x: x[1], reverse=True)

# Format and print results
print("\nAPT Ranking using Total Variation Distance:")
print("="*110)
print(f"{'Rank':<5} | {'APT':<30} | {'Encoded ID':<10} | {'Score':<8} | {'Techniques':<10} | {'Software':<8} | {'CAPEC':<6} | {'CWE':<4} | {'CVE':<4} | {'Pulse':<5}")
print("-"*110)

for rank, (apt_name, score) in enumerate(ranked_results, 1):
    apt_data = apt_profiles[apt_name]
    common_features = {
        'techniques': len(set(encoded_evidence.get('techniques', [])) & set(apt_data['techniques'])),
        'software': len(set(encoded_evidence.get('software', [])) & set(apt_data['software'])),
        'capec': len(set(encoded_evidence.get('capec', [])) & set(apt_data['capec'])),
        'cwe': len(set(encoded_evidence.get('cwe', [])) & set(apt_data['cwe'])),
        'cve': len(set(encoded_evidence.get('cve', [])) & set(apt_data['cve'])),
        'pulse': len(set(encoded_evidence.get('pulse', [])) & set(apt_data['pulse']))
    }
    
    # Truncate long APT names to fit column width
    display_name = (apt_name[:27] + '..') if len(apt_name) > 29 else apt_name
    print(f"{rank:<5} | {display_name:<30} | {apt_data['encoded_id']:<10} | {score:.4f} | {common_features['techniques']:<10} | {common_features['software']:<8} | {common_features['capec']:<6} | {common_features['cwe']:<4} | {common_features['cve']:<4} | {common_features['pulse']:<5}")




# Add these imports at the top of the script if not already present
import matplotlib.pyplot as plt
import seaborn as sns

# ==============================================
# SIMILARITY MATRIX GENERATION
# ==============================================

def calculate_tvd_matrix(apt_profiles, feature_weights):
    """Calculate TVD similarity matrix for all APT pairs"""
    num_apts = len(apt_profiles)
    apt_names = list(apt_profiles.keys())
    similarity_matrix = np.zeros((num_apts, num_apts))
    
    # First get all possible features to create probability distributions
    all_features = {
        'techniques': set(),
        'software': set(),
        'capec': set(),
        'cwe': set(),
        'cve': set(),
        'pulse': set()
    }
    
    # Collect all unique features across all APTs
    for apt_name in apt_profiles:
        for feature_type in all_features:
            all_features[feature_type].update(apt_profiles[apt_name][feature_type])
    
    # Convert to lists for indexing
    feature_indices = {ft: list(features) for ft, features in all_features.items()}
    
    print("Computing TVD similarity matrix...")
    for i in range(num_apts):
        apt1_name = apt_names[i]
        apt1_data = apt_profiles[apt1_name]
        
        for j in range(i, num_apts):  # Only compute upper triangle
            apt2_name = apt_names[j]
            apt2_data = apt_profiles[apt2_name]
            
            total_similarity = 0
            total_weight = 0
            
            for feature_type, weight in feature_weights.items():
                # Create probability distributions
                apt1_features = set(apt1_data[feature_type])
                apt2_features = set(apt2_data[feature_type])
                all_possible = feature_indices[feature_type]
                
                # Create binary vectors (1 if feature present, 0 otherwise)
                apt1_vec = np.array([1 if f in apt1_features else 0 for f in all_possible])
                apt2_vec = np.array([1 if f in apt2_features else 0 for f in all_possible])
                
                # Normalize to probability distributions
                apt1_dist = apt1_vec / apt1_vec.sum() if apt1_vec.sum() > 0 else apt1_vec
                apt2_dist = apt2_vec / apt2_vec.sum() if apt2_vec.sum() > 0 else apt2_vec
                
                # Calculate TVD (half the L1 distance between distributions)
                tvd = 0.5 * np.sum(np.abs(apt1_dist - apt2_dist))
                
                # Convert to similarity (1 - TVD)
                feature_similarity = (1 - tvd) * weight
                total_similarity += feature_similarity
                total_weight += weight
            
            # Normalize by total weight
            normalized_similarity = total_similarity / total_weight if total_weight > 0 else 0
            similarity_matrix[i, j] = normalized_similarity
            similarity_matrix[j, i] = normalized_similarity  # Symmetric matrix
    
    print("TVD similarity matrix computation completed!")
    return similarity_matrix

# Calculate the full similarity matrix
tvd_similarity_matrix = calculate_tvd_matrix(apt_profiles, feature_weights)

# Get display names for APTs
display_names = [apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_profiles.keys()]

# ==============================================
# VISUALIZATION
# ==============================================

# Create the heatmap
plt.figure(figsize=(16, 14))
sns.set(font_scale=0.7)

ax = sns.heatmap(
    tvd_similarity_matrix,
    cmap="YlOrRd",
    xticklabels=display_names,
    yticklabels=display_names,
    annot=False,  # Don't show values to avoid clutter
    square=True
)

# Adjust layout
plt.title("APT Group Similarity Matrix (Total Variation Distance)", fontsize=14)
plt.xticks(rotation=90, fontsize=8)
plt.yticks(rotation=0, fontsize=8)
plt.tight_layout()

# Show the plot
plt.show()

# Also show a text version of the top 10x10 for verification
print("\nText version of TVD similarity matrix (first 10x10):")
print("="*90)
print(f"{'':<20}", end="")
for name in display_names[:10]:
    print(f"{name[:8]:<8}", end="")
print("\n" + "-"*90)

for i in range(10):
    print(f"{display_names[i][:20]:<20}", end="")
    for j in range(10):
        print(f"{tvd_similarity_matrix[i,j]:.2f}  ", end="")
    print()

# ==============================================
# EVIDENCE SIMILARITY RESULTS (existing code remains unchanged)
# ==============================================

# The rest of your existing code for evidence processing and similarity calculation can remain the same
# This includes:
# - The evidence processing (sanitize_evidence and translate_to_encoded)
# - The TVD similarity calculation for evidence
# - The results display