import pandas as pd
import torch
from torch_geometric.data import HeteroData
from torch_geometric.nn import HGTConv, Linear
import torch.nn.functional as F
import numpy as np

# Configuration
pwd = "./DataForGNN/"
model_path = pwd + "apt_gnn_model.pth"
data_path = pwd + "data_for_GNN.csv"



def get_apt_name_mapping():
    """Get mapping from numeric IDs to APT names"""
    df = pd.read_csv(data_path)
    # Get unique APTname entries and their corresponding names
    apt_mapping = df[['APTname']].drop_duplicates()
    apt_mapping = apt_mapping[apt_mapping['APTname'] != -1]
    return apt_mapping['APTname'].tolist()

class APTGNN(torch.nn.Module):
    def __init__(self, hidden_channels, num_heads, metadata):
        super().__init__()
        self.lin_dict = torch.nn.ModuleDict({
            node_type: Linear(16, hidden_channels)
            for node_type in metadata[0]
        })
        
        self.conv1 = HGTConv(hidden_channels, hidden_channels, metadata, num_heads)
        self.conv2 = HGTConv(hidden_channels, hidden_channels, metadata, num_heads)
        self.conv3 = HGTConv(hidden_channels, hidden_channels, metadata, num_heads)
        
        self.apt_classifier = torch.nn.Sequential(
            Linear(hidden_channels, hidden_channels),
            torch.nn.ReLU(),
            Linear(hidden_channels, hidden_channels)
        )

    def forward(self, x_dict, edge_index_dict):
        x_dict = {
            node_type: F.leaky_relu(self.lin_dict[node_type](x))
            for node_type, x in x_dict.items()
        }
        
        x1 = self.conv1(x_dict, edge_index_dict)
        x_dict = {k: F.leaky_relu(v + x_dict[k]) for k, v in x1.items()}
        
        x2 = self.conv2(x_dict, edge_index_dict)
        x_dict = {k: F.leaky_relu(v + x_dict[k]) for k, v in x2.items()}
        
        x3 = self.conv3(x_dict, edge_index_dict)
        x_dict = {k: F.leaky_relu(v + x_dict[k]) for k, v in x3.items()}
        
        return self.apt_classifier(x_dict['APTname'])
    

def load_model():
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    checkpoint = torch.load(model_path, map_location=device)
    
    # Get the parameters from the checkpoint
    hidden_channels = checkpoint.get('hidden_channels', 256)  # Default to 256 if not found
    num_heads = checkpoint.get('num_heads', 8)  # Default to 8 if not found
    
    # Initialize model with the correct parameters
    model = APTGNN(
        hidden_channels=hidden_channels,
        num_heads=num_heads,
        metadata=checkpoint['metadata']
    ).to(device)
    
    model.load_state_dict(checkpoint['model_state_dict'])
    model.eval()
    return model, device, checkpoint



def predict_apt_probabilities(model, device, checkpoint, evidence_dict, comparison_df):
    """Enhanced prediction function with evidence weighting"""
    valid_evidence = {k: v for k, v in evidence_dict.items() 
                     if k in checkpoint['metadata'][0] and v and v != ['']}
    
    if not valid_evidence:
        return []

    # Evidence type weights (adjust these based on importance)
    weights = {
        'techniques_str': 3.0,  # Most distinctive
        'software_str': 2.5,
        'CVE_ID': 2.0,
        'CAPEC_ID': 1.5,
        'CWE_ID': 1.2,
        'Pulse_ID': 1.0
    }

    # Create query graph
    query_data = HeteroData()
    apt_mapping = dict(zip(comparison_df['APTnameEncoded'], comparison_df['APTnameOrigin']))
    encoded_apts = comparison_df['APTnameEncoded'].unique()
    
    # Initialize APT nodes
    query_data['APTname'].x = torch.eye(len(encoded_apts), 16).to(device)

    # Add evidence with weighted features
    for etype, items in valid_evidence.items():
        num_items = len(items)
        query_data[etype].x = torch.ones(num_items, 16).to(device) * weights.get(etype, 1.0)
        
        # Connect to all APTs
        edge_index = torch.cartesian_prod(
            torch.arange(len(encoded_apts)),
            torch.arange(num_items)
        ).t().to(device)
        query_data['APTname', 'relates_to', etype].edge_index = edge_index

    # Get predictions
    with torch.no_grad():
        # Get APT embeddings
        apt_embeddings = model(query_data.x_dict, query_data.edge_index_dict)
        
        # Calculate evidence-specific scores
        scores = torch.zeros(len(encoded_apts), device=device)
        
        for etype in valid_evidence:
            if etype not in query_data.x_dict:
                continue
                
            e_emb = model.lin_dict[etype](query_data[etype].x)
            weight = weights.get(etype, 1.0)
            
            for apt_idx in range(len(encoded_apts)):
                edges = query_data['APTname', 'relates_to', etype].edge_index
                e_indices = edges[1][edges[0] == apt_idx]
                
                if len(e_indices) > 0:
                    apt_emb = apt_embeddings[apt_idx]
                    evidence_embs = e_emb[e_indices]
                    
                    # Weighted similarity
                    sim = F.cosine_similarity(
                        apt_emb.unsqueeze(0),
                        evidence_embs
                    ).mean() * weight
                    scores[apt_idx] += sim
        
        # Convert to probabilities with temperature scaling
        probs = F.softmax(scores * 5.0, dim=0) * 100  # Higher temperature for sharper distribution

    # Prepare and sort results
    results = [
        (apt_mapping[apt_enc], float(probs[i]))
        for i, apt_enc in enumerate(encoded_apts)
    ]
    return sorted(results, key=lambda x: x[1], reverse=True)


if __name__ == "__main__":
    # Load model and data
    model, device, checkpoint = load_model()
    comparison = pd.read_csv(pwd+'comparison_Encodedtest_df.csv')
    unique_df = comparison[['APTnameOrigin', 'APTnameEncoded']].drop_duplicates()
    
    # Use evidence that exactly matches metadata
    # the evidence fed is for APT29
    evidence = {
    'techniques_str': ['T1001.002', 'T1003.002', 'T1003.004', 'T1003.006', 'T1005', 'T1016.001', 'T1018', 'T1021.001', 
                      'T1021.002', 'T1021.006', 'T1021.007', 'T1027.001', 'T1027.002', 'T1027.003', 'T1027.006', 
                      'T1036.004', 'T1036.005', 'T1037', 'T1037.004', 'T1047', 'T1048.002', 'T1053.005', 'T1057', 
                      'T1059.001', 'T1059.003', 'T1059.005', 'T1059.006', 'T1059.009', 'T1068', 'T1069.002', 
                      'T1070.004', 'T1070.006', 'T1070.008', 'T1071.001', 'T1074.002', 'T1078', 'T1078.002', 
                      'T1078.003', 'T1078.004', 'T1082', 'T1083', 'T1087.002', 'T1087.004', 'T1090.001', 'T1090.002', 
                      'T1090.003', 'T1090.004', 'T1098.001', 'T1098.002', 'T1098.003', 'T1098.005', 'T1102.002', 
                      'T1105', 'T1110.001', 'T1110.003', 'T1114.002', 'T1133', 'T1136.003', 'T1140', 'T1190', 
                      'T1195.002', 'T1199', 'T1203', 'T1204.001', 'T1204.002', 'T1213', 'T1213.003', 'T1218.005', 
                      'T1218.011', 'T1482', 'T1484.002', 'T1505.003', 'T1528', 'T1539', 'T1546.003', 'T1546.008', 
                      'T1547.001', 'T1548.002', 'T1550.001', 'T1550.003', 'T1550.004', 'T1552.004', 'T1553.002', 
                      'T1553.005', 'T1555.003', 'T1556.007', 'T1558.003', 'T1560.001', 'T1562.001', 'T1562.002', 
                      'T1562.004', 'T1562.008', 'T1566.001', 'T1566.002', 'T1566.003', 'T1568', 'T1573', 'T1583.001', 
                      'T1583.006', 'T1584.001', 'T1585.001', 'T1586.002', 'T1586.003', 'T1587.001', 'T1587.003', 
                      'T1588.002', 'T1589.001', 'T1595.002', 'T1606.001', 'T1606.002', 'T1621', 'T1649', 'T1651', 
                      'T1665'],
    'software_str': ['S0037', 'S0559', 'S0633', 'S0002', 'S0046', 'S0050', 'S0154', 'S0357', 'S0677', 'S0048', 
                    'S0512', 'S0514', 'S0515', 'S0634', 'S0661', 'S0597', 'S0039', 'S0521', 'S0552', 'S0684', 
                    'S0029', 'S0053', 'S0513', 'S0565', 'S0588', 'S0139', 'S0511', 'S0518', 'S0560', 'S0562', 
                    'S0589', 'S1084', 'S0516', 'S0049', 'S0057', 'S0150', 'S0195', 'S0051', 'S0052', 'S0054', 
                    'S0635', 'S0636', 'S0682', 'S0096', 'S0183', 'S0175', 'S0637', 'S0100'],
    'CAPEC_ID': ['CAPEC-204', 'CAPEC-37', 'CAPEC-545', 'CAPEC-647', 'CAPEC-292', 'CAPEC-561', 'CAPEC-572', 'CAPEC-655', 
                'CAPEC-35', 'CAPEC-504', 'CAPEC-543', 'CAPEC-616', 'CAPEC-564', 'CAPEC-573', 'CAPEC-560', 'CAPEC-70', 
                'CAPEC-312', 'CAPEC-313', 'CAPEC-580', 'CAPEC-127', 'CAPEC-497', 'CAPEC-465', 'CAPEC-481', 'CAPEC-49', 
                'CAPEC-565', 'CAPEC-555', 'CAPEC-186', 'CAPEC-443', 'CAPEC-523', 'CAPEC-669', 'CAPEC-673', 'CAPEC-678', 
                'CAPEC-691', 'CAPEC-150', 'CAPEC-650', 'CAPEC-21', 'CAPEC-31', 'CAPEC-558', 'CAPEC-593', 'CAPEC-645', 
                'CAPEC-60', 'CAPEC-474', 'CAPEC-485', 'CAPEC-639', 'CAPEC-206', 'CAPEC-473', 'CAPEC-68', 'CAPEC-509', 
                'CAPEC-578', 'CAPEC-268', 'CAPEC-571', 'CAPEC-163', 'CAPEC-542', 'CAPEC-464'],
    'CWE_ID': ['CWE-524', 'CWE-311', 'CWE-1258', 'CWE-226', 'CWE-312', 'CWE-318', 'CWE-1278', 'CWE-285', 'CWE-200', 
              'CWE-522', 'CWE-294', 'CWE-521', 'CWE-94', 'CWE-272', 'CWE-59', 'CWE-282', 'CWE-270', 'CWE-1021', 
              'CWE-284', 'CWE-307', 'CWE-798', 'CWE-204', 'CWE-208', 'CWE-425', 'CWE-288', 'CWE-732', 'CWE-276', 
              'CWE-693', 'CWE-441', 'CWE-923', 'CWE-257', 'CWE-494', 'CWE-552', 'CWE-287', 'CWE-290', 'CWE-302', 
              'CWE-346', 'CWE-539', 'CWE-384', 'CWE-664', 'CWE-602', 'CWE-113', 'CWE-20', 'CWE-472', 'CWE-488', 
              'CWE-330', 'CWE-327', 'CWE-325', 'CWE-117', 'CWE-451', 'CWE-359'],
    'CVE_ID': ['CVE-2021-44228'],  # Kept your example CVE
    'Pulse_ID': ['']
    }
    
    #the evidence fed is for Lazarus Group
    # evidence = {
    #     'techniques_str': [
    #         'T0865', 'T1001.003', 'T1005', 'T1008', 'T1010', 'T1012', 'T1016', 
    #         'T1021.001', 'T1021.002', 'T1021.004', 'T1027.002', 'T1027.007', 
    #         'T1027.013', 'T1033', 'T1036.003', 'T1036.004', 'T1036.005', 
    #         'T1036.008', 'T1041', 'T1046', 'T1047', 'T1048.003', 'T1049', 
    #         'T1053.005', 'T1055.001', 'T1056.001', 'T1057', 'T1059.001', 
    #         'T1059.003', 'T1059.005', 'T1070', 'T1070.003', 'T1070.004', 
    #         'T1070.006', 'T1071.001', 'T1074.001', 'T1078', 'T1082', 'T1083', 
    #         'T1087.002', 'T1090.001', 'T1090.002', 'T1098', 'T1102.002', 
    #         'T1104', 'T1105', 'T1106', 'T1110.003', 'T1124', 'T1132.001', 
    #         'T1134.002', 'T1140', 'T1189', 'T1202', 'T1203', 'T1204.001', 
    #         'T1204.002', 'T1218', 'T1218.005', 'T1218.010', 'T1218.011', 
    #         'T1220', 'T1221', 'T1485', 'T1489', 'T1491.001', 'T1497.001', 
    #         'T1497.003', 'T1505.004', 'T1529', 'T1534', 'T1542.003', 
    #         'T1543.003', 'T1547.001', 'T1547.009', 'T1553.002', 'T1557.001', 
    #         'T1560', 'T1560.001', 'T1560.002', 'T1560.003', 'T1561.001', 
    #         'T1561.002', 'T1562.001', 'T1562.004', 'T1564.001', 'T1566.001', 
    #         'T1566.002', 'T1566.003', 'T1567.002', 'T1571', 'T1573.001', 
    #         'T1574.002', 'T1574.013', 'T1583.001', 'T1583.004', 'T1583.006', 
    #         'T1584.001', 'T1584.004', 'T1585.001', 'T1585.002', 'T1587.001', 
    #         'T1587.002', 'T1588.002', 'T1588.003', 'T1588.004', 'T1589.002', 
    #         'T1591', 'T1591.004', 'T1593.001', 'T1608.001', 'T1608.002', 
    #         'T1614.001', 'T1620', 'T1622', 'T1656'
    #     ],
    #     'software_str': [
    #         'S0181', 'S0239', 'S0245', 'S0246', 'S0586', 'S0238', 'S0498', 
    #         'S0520', 'S0567', 'S0665', 'S0694', 'S0376', 'S0431', 'S0180', 
    #         'S0241', 'S0103', 'S0271', 'S0366', 'S0678', 'S0263', 'S0347', 
    #         'S0497', 'S0584', 'S0593', 'S0364', 'S0174', 'S0108'
    #     ],
    #     'CAPEC_ID': [
    #         'CAPEC-204', 'CAPEC-37', 'CAPEC-545', 'CAPEC-647', 'CAPEC-309', 
    #         'CAPEC-561', 'CAPEC-577', 'CAPEC-165', 'CAPEC-504', 'CAPEC-543', 
    #         'CAPEC-616', 'CAPEC-300', 'CAPEC-568', 'CAPEC-573', 'CAPEC-268', 
    #         'CAPEC-560', 'CAPEC-70', 'CAPEC-312', 'CAPEC-313', 'CAPEC-580', 
    #         'CAPEC-127', 'CAPEC-497', 'CAPEC-465', 'CAPEC-565', 'CAPEC-295', 
    #         'CAPEC-196', 'CAPEC-698', 'CAPEC-163', 'CAPEC-552', 'CAPEC-478', 
    #         'CAPEC-132', 'CAPEC-206', 'CAPEC-473', 'CAPEC-68', 'CAPEC-578', 
    #         'CAPEC-641', 'CAPEC-640', 'CAPEC-542'
    #     ],
    #     'CWE_ID': [
    #         'CWE-524', 'CWE-311', 'CWE-1258', 'CWE-226', 'CWE-312', 'CWE-318', 
    #         'CWE-1278', 'CWE-285', 'CWE-200', 'CWE-522', 'CWE-294', 'CWE-521', 
    #         'CWE-1021', 'CWE-117', 'CWE-307', 'CWE-798', 'CWE-204', 'CWE-208', 
    #         'CWE-425', 'CWE-288', 'CWE-732', 'CWE-276', 'CWE-693', 'CWE-441', 
    #         'CWE-384', 'CWE-664', 'CWE-829', 'CWE-451', 'CWE-284', 'CWE-59', 
    #         'CWE-20', 'CWE-327', 'CWE-290', 'CWE-325', 'CWE-706', 'CWE-114'
    #     ],
    #     'CVE_ID': [],  # No CVE data provided in the original
    #     'Pulse_ID': []  # No Pulse data provided in the original
    # }
    
    # Get and print all results
    apt_results = predict_apt_probabilities(model, device, checkpoint, evidence, unique_df)
    
    print("\nAll APTs ranked by probability (%):")
    for rank, (apt, prob) in enumerate(apt_results, 1):
        print(f"{rank:3d}. {apt}: {prob:.2f}%")
    
    