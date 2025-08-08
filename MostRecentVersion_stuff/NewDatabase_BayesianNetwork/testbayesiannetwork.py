import pandas as pd
import numpy as np
from pgmpy.models import DiscreteBayesianNetwork
from pgmpy.estimators import MaximumLikelihoodEstimator
from pgmpy.inference import VariableElimination
from collections import defaultdict
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.metrics import jaccard_score
import matplotlib.pyplot as plt
import seaborn as sns

# Load the data
pwd = "./DataForBayesianNetworkPython/"
df_encoded = pd.read_csv(pwd+'data_for_bn.csv')
df_mapping = pd.read_csv(pwd+'comparison_Encodedtest_df.csv')

# Create APT profiles
apt_name_mapping = df_mapping[['APTnameEncoded', 'APTnameOrigin']].drop_duplicates()
apt_name_dict = dict(zip(apt_name_mapping['APTnameEncoded'], apt_name_mapping['APTnameOrigin']))

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

# Prepare data for Bayesian Network
def prepare_bayesian_data(apt_profiles):
    data = []
    
    for apt_name, profile in apt_profiles.items():
        for feature_type in ['techniques', 'software', 'capec', 'cwe', 'cve', 'pulse']:
            for feature in profile[feature_type]:
                data.append({
                    'APT': apt_name,
                    'Feature_Type': feature_type,
                    'Feature': feature
                })
    
    return pd.DataFrame(data)

# Create Bayesian Network model
def create_bayesian_model(df_bayesian):
    model = DiscreteBayesianNetwork([
        ('APT', 'Feature_Type'),
        ('Feature_Type', 'Feature'),
        ('APT', 'Feature')  # Direct relationship
    ])
    
    model.fit(df_bayesian, estimator=MaximumLikelihoodEstimator)
    return model

# Prepare the data and create model
df_bayesian = prepare_bayesian_data(apt_profiles)
model = create_bayesian_model(df_bayesian)

# Create inference engine
inference = VariableElimination(model)

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
        print("\nWARNING: These features were not found in our database:")
        for feature_type, items in missing.items():
            print(f"- {feature_type}: {items[:5]}")
            if len(items) > 5:
                print(f"  (and {len(items)-5} more)")
        print()
    
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

# Enhanced probability calculation
def calculate_apt_probabilities(encoded_evidence, model, inference, apt_profiles):
    results = []
    min_prob = 1e-10  # Minimum probability to avoid zeros
    
    weights = {
        'techniques': 2.0,
        'software': 1.5,
        'capec': 1.2,
        'cwe': 1.0,
        'cve': 0.8,
        'pulse': 0.5
    }
    
    print("\nEvidence features per type:")
    for k, v in encoded_evidence.items():
        print(f"{k}: {len(v)} features")
    
    for apt_name in apt_profiles.keys():
        total_log_prob = 0
        perfect_matches = 0
        
        for feature_type, features in encoded_evidence.items():
            weight = weights.get(feature_type, 1.0)
            apt_features = set(apt_profiles[apt_name][feature_type])
            evidence_features = set(features)
            
            # Check for perfect match
            if apt_features == evidence_features:
                perfect_matches += 1
            
            for feature in features:
                try:
                    query = inference.query(
                        variables=['APT'],
                        evidence={
                            'Feature_Type': feature_type,
                            'Feature': feature
                        }
                    )
                    prob = max(query.values[list(apt_profiles.keys()).index(apt_name)], min_prob)
                    total_log_prob += weight * np.log(prob)
                except:
                    continue
        
        # Calculate final probability with perfect match boost
        if perfect_matches > 0:
            total_log_prob *= (5 ** perfect_matches)  # Exponential boost for perfect matches
            
        results.append({
            'APT': apt_name,
            'Probability': np.exp(total_log_prob),
            'Encoded_ID': apt_profiles[apt_name]['encoded_id'],
            'Perfect_Matches': perfect_matches
        })
    
    return pd.DataFrame(results)

# Create similarity matrix
def create_similarity_matrix(apt_profiles, encoded_evidence):
    apt_names = list(apt_profiles.keys())
    num_apts = len(apt_names)
    similarity_matrix = np.zeros((num_apts, num_apts))
    
    for i in range(num_apts):
        for j in range(num_apts):
            if i <= j:  # Only compute upper triangle
                # Create binary vectors for each APT
                vec_i = []
                vec_j = []
                
                for feature_type in ['techniques', 'software', 'capec', 'cwe', 'cve', 'pulse']:
                    if feature_type in encoded_evidence:
                        features_i = set(apt_profiles[apt_names[i]][feature_type])
                        features_j = set(apt_profiles[apt_names[j]][feature_type])
                        all_features = set(encoded_evidence[feature_type])
                        
                        vec_i.extend([1 if f in features_i else 0 for f in all_features])
                        vec_j.extend([1 if f in features_j else 0 for f in all_features])
                
                if len(vec_i) > 0 and len(vec_j) > 0:
                    similarity = jaccard_score(vec_i, vec_j, average='micro')
                    similarity_matrix[i,j] = similarity
                    similarity_matrix[j,i] = similarity
    
    # Create DataFrame with APT names
    apt_display_names = [apt_name_dict[apt_profiles[name]['encoded_id']] for name in apt_names]
    similarity_df = pd.DataFrame(similarity_matrix, index=apt_display_names, columns=apt_display_names)
    
    return similarity_df

# Updated final ranking calculation
def calculate_final_ranking(results_df, apt_profiles, encoded_evidence, similarity_df):
    # Calculate match percentages
    match_data = []
    for _, row in results_df.iterrows():
        apt_name = row['APT']
        matches = {}
        total_possible = {}
        
        for feature_type in encoded_evidence.keys():
            apt_features = set(apt_profiles[apt_name][feature_type])
            evidence_features = set(encoded_evidence[feature_type])
            matches[feature_type] = len(apt_features & evidence_features)
            total_possible[feature_type] = len(evidence_features)
        
        match_percentage = sum(matches.values()) / sum(total_possible.values()) if sum(total_possible.values()) > 0 else 0
        complete_matches = sum([1 for ft in matches.keys() if matches[ft] == total_possible[ft]])
        
        match_data.append({
            'APT': apt_name,
            'Match_Percentage': match_percentage,
            'Complete_Matches': complete_matches
        })
    
    match_df = pd.DataFrame(match_data)
    final_df = results_df.merge(match_df, on='APT')
    
    # Add similarity scores
    apt_names = [apt_name_dict[apt_profiles[name]['encoded_id']] for name in final_df['APT']]
    final_df['Similarity'] = [similarity_df.loc[name, 'Lazarus Group'] if 'Lazarus Group' in similarity_df.columns else 0 for name in apt_names]
    
    # Modified combined score calculation
    # Give highest priority to similarity, then perfect matches, then match percentage
    final_df['Combined_Score'] = (
        (final_df['Probability'] + 1e-10) *  # Small epsilon to avoid zero
        (1 + final_df['Similarity'] * 10) *  # Heavily weight similarity
        (1 + final_df['Complete_Matches'] * 5) *  # Boost for perfect matches
        (1 + final_df['Match_Percentage'])  # Consider overall match percentage
    )
    
    # Special case: if an APT has perfect similarity (1.0), put it at the top
    perfect_similarity = final_df['Similarity'] == 1.0
    final_df['Rank_Score'] = np.where(
        perfect_similarity,
        1000 + final_df['Complete_Matches'] * 100 + final_df['Match_Percentage'] * 10,
        final_df['Combined_Score']
    )
    
    # Sort by the ranking score
    final_df = final_df.sort_values('Rank_Score', ascending=False)
    final_df['Rank'] = range(1, len(final_df)+1)
    
    return final_df.drop('Rank_Score', axis=1)

# Example evidence for Lazarus Group
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

# Calculate probabilities
results_df = calculate_apt_probabilities(encoded_evidence, model, inference, apt_profiles)

# Create similarity matrix
similarity_df = create_similarity_matrix(apt_profiles, encoded_evidence)

# Calculate final rankings with updated function
final_results = calculate_final_ranking(results_df, apt_profiles, encoded_evidence, similarity_df)

# Add APT names - FIXED LINE
final_results['APT_Name'] = final_results['APT'].apply(lambda x: apt_name_dict[apt_profiles[x]['encoded_id']])

# Display all results
pd.set_option('display.max_rows', None)
pd.set_option('display.float_format', '{:.6f}'.format)
print("\nFinal Rankings:")
print(final_results[['Rank', 'APT_Name', 'Combined_Score', 'Probability', 
                   'Match_Percentage', 'Complete_Matches', 'Similarity']])

# Plot similarity matrix
plt.figure(figsize=(20, 18))
sns.heatmap(similarity_df, cmap="YlOrRd", vmin=0, vmax=1)
plt.title("APT Group Similarity Matrix (Jaccard Similarity)", fontsize=16)
plt.xticks(rotation=90, fontsize=6)
plt.yticks(fontsize=6)
plt.tight_layout()
plt.show()