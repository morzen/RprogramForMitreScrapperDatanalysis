import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import Dataset, DataLoader
from collections import defaultdict
import numpy as np

# Data loading and preprocessing
pwd = "./DataForMIL/"
df_encoded = pd.read_csv(pwd+'data_for_MIL.csv')
df_mapping = pd.read_csv(pwd+'comparison_Encodedtest_df.csv')

# Create APT name mapping
apt_name_mapping = df_mapping[['APTnameEncoded', 'APTnameOrigin']].drop_duplicates()
apt_name_dict = dict(zip(apt_name_mapping['APTnameEncoded'], apt_name_mapping['APTnameOrigin']))

# Create APT profiles
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

# Get list of all APT names
apt_ids = list(apt_profiles.keys())

# Evidence processing
#for APT29
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

clean_evidence = sanitize_evidence(evidence, df_mapping)
encoded_evidence = translate_to_encoded(clean_evidence, df_mapping)

class OptimizedAttentionMILSimilarity(nn.Module):
    def __init__(self, hidden_dim=256):
        super().__init__()
        self.hidden_dim = hidden_dim
        
        # Feature embedding layers
        self.technique_embed = nn.Embedding(10000, hidden_dim)
        self.software_embed = nn.Embedding(10000, hidden_dim)
        self.capec_embed = nn.Embedding(10000, hidden_dim)
        self.cwe_embed = nn.Embedding(10000, hidden_dim)
        self.cve_embed = nn.Embedding(10000, hidden_dim)
        self.pulse_embed = nn.Embedding(10000, hidden_dim)
        
        # Attention networks
        self.attention_nets = nn.ModuleDict({
            'technique': self._build_attention(),
            'software': self._build_attention(),
            'capec': self._build_attention(),
            'cwe': self._build_attention(),
            'cve': self._build_attention(),
            'pulse': self._build_attention()
        })
        
        # Similarity network
        self.similarity_net = nn.Sequential(
            nn.Linear(hidden_dim * 6 * 2 + 6, hidden_dim*4),  # +6 for match ratios
            nn.LayerNorm(hidden_dim*4),
            nn.ReLU(),
            nn.Linear(hidden_dim*4, hidden_dim*2),
            nn.LayerNorm(hidden_dim*2),
            nn.ReLU(),
            nn.Linear(hidden_dim*2, 6),
            nn.Sigmoid()
        )
        
        # Initialize weights with higher technique importance
        self.feature_weights = nn.Parameter(torch.tensor([0.4, 0.3, 0.15, 0.1, 0.025, 0.025]))
        self.perfect_match_boost = 2.0  # Direct score multiplier for perfect matches
        
        self.init_weights()  # Changed from _init_weights to init_weights

    def _build_attention(self):
        return nn.Sequential(
            nn.Linear(self.hidden_dim, self.hidden_dim),
            nn.Tanh(),
            nn.Linear(self.hidden_dim, 1)
        )

    def init_weights(self):
        """Initialize weights for embeddings and linear layers"""
        for embed in [self.technique_embed, self.software_embed, self.capec_embed, 
                     self.cwe_embed, self.cve_embed, self.pulse_embed]:
            nn.init.xavier_uniform_(embed.weight)
        
        for net in self.attention_nets.values():
            for layer in net:
                if isinstance(layer, nn.Linear):
                    nn.init.xavier_uniform_(layer.weight)
                    nn.init.zeros_(layer.bias)
        
        for layer in self.similarity_net:
            if isinstance(layer, nn.Linear):
                nn.init.xavier_uniform_(layer.weight)
                nn.init.zeros_(layer.bias)

    def _calculate_match_ratios(self, apt_dict, evidence_dict):
        ratios = []
        perfect_matches = []
        feature_types = ['technique', 'software', 'capec', 'cwe', 'cve', 'pulse']
        
        for feat_type in feature_types:
            dict_key = feat_type + 's' if feat_type != 'software' else feat_type
            apt_features = set(apt_dict.get(dict_key, []))
            evidence_features = set(evidence_dict.get(dict_key, []))
            
            if len(evidence_features) == 0:
                ratios.append(0.01)
                perfect_matches.append(0)
            else:
                coverage = len(apt_features & evidence_features) / len(evidence_features)
                is_perfect = 1 if coverage == 1.0 else 0
                perfect_matches.append(is_perfect)
                
                # Linear coverage with small minimum
                ratios.append(max(coverage, 0.01))
                
        return torch.tensor(ratios), torch.tensor(perfect_matches)

    def forward(self, x1_dict, x2_dict):
        match_ratios, perfect_matches = self._calculate_match_ratios(x1_dict, x2_dict)
        emb1 = self._forward_single(x1_dict, match_ratios)
        emb2 = self._forward_single(x2_dict, match_ratios)
        
        combined = torch.cat([emb1, emb2, match_ratios.unsqueeze(0)], dim=-1)
        feature_similarities = self.similarity_net(combined)
        
        # Apply perfect match boost directly to scores
        boosted_similarities = feature_similarities * (1 + perfect_matches * (self.perfect_match_boost - 1))
        
        # Use softmax weights
        weights = F.softmax(self.feature_weights, dim=0)
        
        # Final score
        similarity = (boosted_similarities * weights).sum(dim=-1, keepdim=True)
        return torch.sigmoid(similarity * 10)

    def _forward_single(self, x_dict, match_ratios):
        features = []
        feature_types = ['technique', 'software', 'capec', 'cwe', 'cve', 'pulse']
        
        for i, feat_type in enumerate(feature_types):
            dict_key = feat_type + 's' if feat_type != 'software' else feat_type
            feat = x_dict.get(dict_key, [])
            
            if len(feat) > 0:
                embedded = getattr(self, f'{feat_type}_embed')(
                    torch.tensor(feat, dtype=torch.long))
                
                # Enhanced attention with sigmoid gate
                attn_weights = F.softmax(
                    self.attention_nets[feat_type](embedded), dim=0)
                attended = (embedded * attn_weights).sum(dim=0, keepdim=True)
                
                # Weight by both match ratio and log count
                weight = torch.log1p(torch.tensor(len(feat))) * (match_ratios[i] + 0.1)
                features.append(attended * weight)
            else:
                features.append(torch.zeros(1, self.hidden_dim))
                
        return torch.cat(features, dim=-1)

# Initialize the optimized model
model = OptimizedAttentionMILSimilarity()

# Prepare evidence features
evidence_features = {
    'techniques': encoded_evidence.get('techniques', []),
    'software': encoded_evidence.get('software', []),
    'capec': encoded_evidence.get('capec', []),
    'cwe': encoded_evidence.get('cwe', []),
    'cve': encoded_evidence.get('cve', []),
    'pulse': encoded_evidence.get('pulse', [])
}

# Verify evidence alignment with APT29


# Calculate similarities for all APTs
similarities = {}
with torch.no_grad():
    for apt_name in apt_ids:
        apt_features = {
            'techniques': apt_profiles[apt_name]['techniques'],
            'software': apt_profiles[apt_name]['software'],
            'capec': apt_profiles[apt_name]['capec'],
            'cwe': apt_profiles[apt_name]['cwe'],
            'cve': apt_profiles[apt_name]['cve'],
            'pulse': apt_profiles[apt_name]['pulse']
        }
        
        similarity = model(apt_features, evidence_features)
        similarities[apt_name] = similarity.item()

# Sort APTs by similarity score
sorted_apts = sorted(similarities.items(), key=lambda x: x[1], reverse=True)

# Prepare results
full_results = []
for rank, (apt_name, score) in enumerate(sorted_apts, 1):
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
        'APT': apt_name,
        'Score': score,
        'Encoded_ID': apt_data['encoded_id'],
        **common_features
    })

# Convert to DataFrame
ranked_apts = pd.DataFrame(full_results)

# Display results
pd.set_option('display.max_rows', None)
pd.set_option('display.width', 1000)

print("\nComplete APT Ranking (1-181) with Feature Matches:")
print("="*110)
print(f"{'Rank':<5} | {'APT':<30} | {'Encoded ID':<10} | {'Score':<8} | {'Techniques':<10} | {'Software':<8} | {'CAPEC':<6} | {'CWE':<4} | {'CVE':<4} | {'Pulse':<5}")
print("-"*110)

for _, row in ranked_apts.iterrows():
    apt_name = (row['APT'][:27] + '..') if len(row['APT']) > 29 else row['APT']
    print(f"{row['Rank']:<5} | {apt_name:<30} | {row['Encoded_ID']:<10} | {row['Score']:.4f} | {row['techniques']:<10} | {row['software']:<8} | {row['capec']:<6} | {row['cwe']:<4} | {row['cve']:<4} | {row['pulse']:<5}")




####matrix
import matplotlib.pyplot as plt
import seaborn as sns

# Generate full similarity matrix for all APT pairs
num_apts = len(apt_ids)
similarity_matrix = np.zeros((num_apts, num_apts))

with torch.no_grad():
    print("Computing similarity matrix...")
    for i in range(num_apts):
        apt1_name = apt_ids[i]
        apt1_features = {
            'techniques': apt_profiles[apt1_name]['techniques'],
            'software': apt_profiles[apt1_name]['software'],
            'capec': apt_profiles[apt1_name]['capec'],
            'cwe': apt_profiles[apt1_name]['cwe'],
            'cve': apt_profiles[apt1_name]['cve'],
            'pulse': apt_profiles[apt1_name]['pulse']
        }
        
        for j in range(i, num_apts):  # Only compute upper triangle to save computation
            apt2_name = apt_ids[j]
            apt2_features = {
                'techniques': apt_profiles[apt2_name]['techniques'],
                'software': apt_profiles[apt2_name]['software'],
                'capec': apt_profiles[apt2_name]['capec'],
                'cwe': apt_profiles[apt2_name]['cwe'],
                'cve': apt_profiles[apt2_name]['cve'],
                'pulse': apt_profiles[apt2_name]['pulse']
            }
            
            similarity = model(apt1_features, apt2_features)
            similarity_matrix[i, j] = similarity.item()
            similarity_matrix[j, i] = similarity_matrix[i, j]  # Symmetric matrix
    print("Similarity matrix computation completed!")

# Get APT names in order (using encoded_id to name mapping)
apt_names = [apt_name_dict[apt_profiles[apt]['encoded_id']] for apt in apt_ids]

# Create DataFrame for display
similarity_df = pd.DataFrame(
    similarity_matrix,
    index=apt_names,
    columns=apt_names
)

# Display the matrix using matplotlib
plt.figure(figsize=(16, 14))
sns.set(font_scale=0.7)

# Create the heatmap
ax = sns.heatmap(
    similarity_matrix,
    cmap="YlOrRd",
    xticklabels=apt_names,
    yticklabels=apt_names,
    annot=False,  # Don't show values in cells to avoid clutter
    square=True
)

# Adjust layout
plt.title("APT Group Similarity Matrix (Attention MIL Model)", fontsize=14)
plt.xticks(rotation=90, fontsize=8)
plt.yticks(rotation=0, fontsize=8)
plt.tight_layout()

# Show the plot
plt.show()