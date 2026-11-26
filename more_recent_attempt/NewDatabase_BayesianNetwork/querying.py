import pickle
from pgmpy.inference import VariableElimination

# Load the saved model
with open("bayesian_model.pkl", "rb") as f:
    bn_model = pickle.load(f)

# Initialize inference engine
infer = VariableElimination(bn_model)

# Example: show all nodes
print("Available nodes in model:")
print(list(bn_model.nodes()))

# --- Example Query ---
# Change this to a node from your model
query_node = list(bn_model.nodes())[0]

print(f"\nRunning inference on: {query_node}")
result = infer.query(variables=[query_node])
print(result)
