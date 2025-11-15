import torch
import torch.nn.functional as F
from torch.nn import Linear, Sequential, ReLU
import networkx as nx
import igraph as ig
import matplotlib.pyplot as plt
from torch_geometric.nn import NNConv, BatchNorm, summary
from torch_geometric.utils import from_networkx
import numpy as np
import geopandas as gpd
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.manifold import TSNE
import copy

# -------------------------------
# Load spatial and claims data
# -------------------------------
pc_map = gpd.read_file('./Data/Belgium/Belgium.shp')  # Shapefile of Belgian postal codes
claims = pd.read_csv("./Data/Belgium/claims_belgium.csv")  # Claims dataset

# Aggregate claims by postal code
claims_agg = claims.groupby('pc', as_index=False).apply(
    lambda x: pd.Series({
        'exposure': np.nansum(x.exposure),  # Sum exposure for each postal code
        'amount': np.nansum(x.amount),      # Sum claim amount for each postal code
    }),
    include_groups=False
).assign(
    loss_cost=lambda x: np.where(x.exposure == 0, 0, x.amount / x.exposure)  # Compute loss cost
)

# Merge aggregated claims with the shapefile geometries
pc_claims = pc_map.merge(claims_agg, left_on='POSTCODE', right_on='pc', how='left')

# Convert to GeoDataFrame with correct CRS
pc_claims_gpd = gpd.GeoDataFrame(pc_claims, geometry=pc_claims.geometry, crs='WGS84')

# Plot initial map showing postal code boundaries
pc_claims_gpd.boundary.plot(color="black")
plt.show()

# -------------------------------
# Create graph based on spatial adjacency
# -------------------------------
pc_claims_gpd = pc_claims_gpd.to_crs(31370)  # Project to Belgian Lambert CRS for distance calculations
spatial_index = pc_claims_gpd.sindex  # Spatial index for efficient neighbor search

# Make all geometries valid
valid_geoms = pc_claims_gpd.geometry.make_valid()

border_lengths = {}  # Store lengths of borders between neighbors
adj_dict = {}        # Store adjacency list

# Iterate over all polygons to find neighbors
for i, geom_i in enumerate(valid_geoms):
    candidates = list(spatial_index.intersection(geom_i.bounds))  # Candidate neighbors via bounding boxes
    neighbors = []
    i_dict = {}
    for j in candidates:
        if i == j:
            continue
        geom_j = valid_geoms.iloc[j]
        if geom_i.touches(geom_j):  # Check if polygons share a border
            inter = geom_i.intersection(geom_j)
            if not inter.is_empty:
                neighbors.append(j)
                i_dict[j] = inter.length  # Store border length
    adj_dict[i] = np.array(neighbors, dtype=int)
    border_lengths[i] = i_dict

# Create NetworkX graph from adjacency list
graph = nx.Graph(adj_dict)

# Compute centroids for plotting
centr = pc_claims_gpd.centroid.to_crs(4326).to_frame(name = 'geometry').reset_index(drop=True)
pos = {i: np.array([centr.loc[i, 'geometry'].x, centr.loc[i, 'geometry'].y]) for i in range(len(pc_map.geometry))}

# Draw the graph
nx.draw(graph, pos=pos, node_size=15)
plt.show()

# -------------------------------
# Node centrality features
# -------------------------------
deg_cent = nx.degree_centrality(graph)        # Degree centrality
clo_cent = nx.closeness_centrality(graph)    # Closeness centrality
bet_cent = nx.betweenness_centrality(graph)  # Betweenness centrality
eig_cent = nx.eigenvector_centrality(graph, max_iter=1000, tol=1e-06)  # Eigenvector centrality
pagerank = nx.pagerank(graph)                # PageRank score

# Combine all centrality features into a single array
centrality_features = np.array(
    [[deg_cent[n], clo_cent[n], bet_cent[n], eig_cent[n], pagerank[n]] for n in graph.nodes()],
    dtype=np.float32
)

# -------------------------------
# Community detection (clusters)
# -------------------------------
graph_ig = ig.Graph.from_networkx(graph)  # Convert to igraph for community detection
dendrogram = graph_ig.community_fastgreedy()  # Fast greedy clustering
clusters = dendrogram.as_clustering()
cluster_vector = np.array(clusters.membership)  # Cluster ID for each node
num_clusters = cluster_vector.max() + 1
cluster_one_hot = np.eye(num_clusters)[cluster_vector].astype(np.float32)  # One-hot encode clusters

# Normalize centroid coordinates for node features
centr_x = pc_claims_gpd.geometry.centroid.x
centr_y = pc_claims_gpd.geometry.centroid.y
centr_x = (centr_x - centr_x.mean()) / centr_x.std()
centr_y = (centr_y - centr_y.mean()) / centr_y.std()

# Normalize area and perimeter
areas = pc_claims_gpd.geometry.area
pers = pc_claims_gpd.geometry.length
areas = (areas - areas.mean()) / areas.std()
pers = (pers - pers.mean()) / pers.std()

# -------------------------------
# Combine all node features
# -------------------------------
node_feats = np.hstack([
    centrality_features,                  # Centrality features
    centr_x.values.reshape(-1, 1),       # Normalized x centroid
    centr_y.values.reshape(-1, 1),       # Normalized y centroid
    areas.values.reshape(-1, 1),         # Normalized area
    pers.values.reshape(-1, 1),          # Normalized perimeter
    cluster_one_hot                       # One-hot clusters
])
node_feats = np.array(node_feats, dtype=np.float32)
node_feats = (node_feats - node_feats.mean(axis=0)) / node_feats.std(axis=0)  # Standardize features

# -------------------------------
# Edge features
# -------------------------------
edges = []
edge_feats = []
centr_x_vals = pc_claims_gpd.geometry.centroid.x.values
centr_y_vals = pc_claims_gpd.geometry.centroid.y.values
edge_cent = nx.edge_betweenness_centrality(graph)  # Edge centrality

# Compute features for each edge
for i, nbrs in border_lengths.items():
    for j, border_len in nbrs.items():
        if i < j:  # Avoid duplicate edges
            edges.append((i, j))
            dx = centr_x_vals[i] - centr_x_vals[j]
            dy = centr_y_vals[i] - centr_y_vals[j]
            dist = np.sqrt(dx**2 + dy**2)  # Euclidean distance between centroids
            e_cent = edge_cent[(i, j)] if (i, j) in edge_cent else edge_cent[(j, i)]
            edge_feats.append([border_len, dist, e_cent])

edge_feats = np.array(edge_feats, dtype=np.float32)
edge_feats = (edge_feats - edge_feats.mean(axis=0)) / edge_feats.std(axis=0)  # Standardize edges

# -------------------------------
# Assign features to nodes and edges
# -------------------------------
for i, n in enumerate(graph.nodes()):
    graph.nodes[n]['feat'] = node_feats[i]  # Node feature
for (i, j), feat in zip(edges, edge_feats):
    graph.edges[i, j]['feat'] = feat       # Edge feature

# -------------------------------
# Add centrality and cluster features to GeoDataFrame
# -------------------------------
features_df = pd.DataFrame({
    'deg': deg_cent.values(),
    'clo': clo_cent.values(),
    'bet': bet_cent.values(),
    'eig': eig_cent.values(),
    'pagerank': pagerank.values(),
    'cluster': cluster_vector
})
features_df.index = pc_claims_gpd.index
pc_claims_gpd = pd.concat([pc_claims_gpd, features_df], axis=1)

# -------------------------------
# Train/validation/test split
# -------------------------------
all_idx = np.where(~np.isnan(pc_claims_gpd.exposure))[0]  # Indices with exposure
train_idx, test_idx = train_test_split(all_idx, test_size=0.3, train_size=0.7)
train_idx, val_idx = train_test_split(train_idx, test_size=0.3, train_size=0.7)

# Visualize subgraphs for each split
for idx_set in [train_idx, val_idx, test_idx]:
    subgraph = graph.subgraph(idx_set)
    nx.draw(subgraph, pos={n: pos[n] for n in idx_set}, node_size=15)
    plt.show()

# Assign data types
pc_claims_gpd["data_type"] = "no_exp"
pc_claims_gpd.loc[train_idx, "data_type"] = "train"
pc_claims_gpd.loc[val_idx, "data_type"] = "val"
pc_claims_gpd.loc[test_idx, "data_type"] = "test"

# Masks for PyG
train_mask = torch.tensor([i in train_idx for i in range(len(pc_claims_gpd))], dtype=torch.bool)
val_mask = torch.tensor([i in val_idx for i in range(len(pc_claims_gpd))], dtype=torch.bool)
test_mask = torch.tensor([i in test_idx for i in range(len(pc_claims_gpd))], dtype=torch.bool)

# -------------------------------
# Convert NetworkX graph to PyG dataset
# -------------------------------
dataset = from_networkx(graph, group_node_attrs=['feat'], group_edge_attrs=['feat'])
dataset.x = dataset.x.float()
dataset.edge_attr = dataset.edge_attr.float()
dataset.y = torch.tensor(pc_claims_gpd.loss_cost.values, dtype=torch.float)
dataset.exposure = torch.tensor(pc_claims_gpd.exposure.values, dtype=torch.float)
dataset.train_mask = train_mask
dataset.val_mask = val_mask
dataset.test_mask = test_mask

# -------------------------------
# Tweedie loss function
# -------------------------------
def tweedie_loss(preds, targets, weights, power=1.5):
    preds = torch.clamp(preds, min=1e-6)
    targets = torch.clamp(targets, min=0)
    term_1 = torch.pow(targets, 2 - power) / ((1 - power) * (2 - power))
    term_2 = targets * torch.pow(preds, 1 - power) / (1 - power)
    term_3 = torch.pow(preds, 2 - power) / (2 - power)
    loss = torch.sum(weights * (2 * (term_1 - term_2 + term_3))) / torch.sum(weights)
    return loss

# -------------------------------
# Graph Convolutional Network
# -------------------------------
class NNConvModel(torch.nn.Module):
    def __init__(self, node_in_dim, edge_in_dim):
        super().__init__()
        # Edge neural networks for NNConv layers
        self.edge_nn1 = Sequential(Linear(edge_in_dim, 64), ReLU(), Linear(64, node_in_dim * 32))
        self.edge_nn2 = Sequential(Linear(edge_in_dim, 128), ReLU(), Linear(128, 32 * 64))
        self.edge_nn3 = Sequential(Linear(edge_in_dim, 256), ReLU(), Linear(256, 64 * 128))

        # Graph convolutions
        self.conv1 = NNConv(node_in_dim, 32, self.edge_nn1)
        self.conv2 = NNConv(32, 64, self.edge_nn2)
        self.conv3 = NNConv(64, 128, self.edge_nn3)

        # Batch normalization
        self.bn1 = BatchNorm(32)
        self.bn2 = BatchNorm(64)
        self.bn3 = BatchNorm(128)

        # Fully connected layers for final output
        self.fnn1 = Linear(128, 64)
        self.fnn2 = Linear(64, 32)
        self.fnn3 = Linear(32, 1)

        self.apply(self.init_weights)  # Initialize weights

    def init_weights(self, m):
        if isinstance(m, Linear):
            torch.nn.init.xavier_uniform_(m.weight)
            if m.bias is not None:
                m.bias.data.fill_(0.0)

    def forward(self, data):
        x, edge_index, edge_attr = data.x, data.edge_index, data.edge_attr

        # NNConv + batch norm + activation + dropout + residual connections
        x1 = self.conv1(x, edge_index, edge_attr)
        x1 = self.bn1(x1)
        x1 = F.leaky_relu(x1, 0.01)
        x1 = F.dropout(x1, p=0.2, training=self.training)
        if x.shape[1] != x1.shape[1]:
            x_pad = torch.zeros((x.shape[0], x1.shape[1] - x.shape[1]), device=x.device)
            x1 = x1 + torch.cat([x, x_pad], dim=1)
        else:
            x1 = x1 + x

        x2 = self.conv2(x1, edge_index, edge_attr)
        x2 = self.bn2(x2)
        x2 = F.leaky_relu(x2, 0.01)
        x2 = F.dropout(x2, p=0.2, training=self.training)
        if x1.shape[1] != x2.shape[1]:
            x_pad = torch.zeros((x1.shape[0], x2.shape[1] - x1.shape[1]), device=x.device)
            x2 = x2 + torch.cat([x1, x_pad], dim=1)
        else:
            x2 = x2 + x1

        x3 = self.conv3(x2, edge_index, edge_attr)
        x3 = self.bn3(x3)
        x3 = F.leaky_relu(x3, 0.01)
        x3 = F.dropout(x3, p=0.2, training=self.training)
        if x2.shape[1] != x3.shape[1]:
            x_pad = torch.zeros((x2.shape[0], x3.shape[1] - x2.shape[1]), device=x.device)
            x3 = x3 + torch.cat([x2, x_pad], dim=1)
        else:
            x3 = x3 + x2
        y = x3  # Node embeddings

        # Fully connected output layers
        x = self.fnn1(x3)
        x = F.leaky_relu(x, 0.01)
        x = F.dropout(x, p=0.2, training=self.training)
        x = self.fnn2(x)
        x = F.leaky_relu(x, 0.01)
        x = F.dropout(x, p=0.2, training=self.training)
        x = self.fnn3(x)

        return y, x  # Return embeddings and final prediction

# -------------------------------
# Training
# -------------------------------
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')  # Use GPU if available
data = dataset.to(device)
node_in_dim = dataset.num_node_features
edge_in_dim = dataset.edge_attr.shape[1]

model = NNConvModel(node_in_dim=node_in_dim, edge_in_dim=edge_in_dim)
optimizer = torch.optim.AdamW(model.parameters(), lr=0.005, weight_decay=1e-4)  # Optimizer
scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(optimizer, mode='min', factor=0.5, patience=20)  # LR scheduler

train_mask, val_mask = data.train_mask.to(device), data.val_mask.to(device)

losses, val_losses = [], []
best_val_loss = float('inf')
patience = 10
counter = 0  # Early stopping counter

for epoch in range(1000):
    model.train()
    optimizer.zero_grad()
    _, out = model(data)  # Forward pass

    # Compute Tweedie loss for training and validation
    loss = tweedie_loss(out[train_mask], data.y[train_mask], data.exposure[train_mask])
    val_loss = tweedie_loss(out[val_mask], data.y[val_mask], data.exposure[val_mask])
    losses.append(loss.item())
    val_losses.append(val_loss.item())

    loss.backward()
    optimizer.step()
    scheduler.step(val_loss)  # Adjust learning rate if validation loss plateaus

    # Early stopping logic
    if val_loss.item() < best_val_loss:
        best_val_loss = val_loss.item()
        best_model_state = copy.deepcopy(model.state_dict())
        counter = 0
    else:
        counter += 1
        if counter >= patience:
            print("Early stopping triggered")
            model.load_state_dict(best_model_state)
            break

    if epoch % 10 == 0:
        print(f"Epoch {epoch}: Tweedie Loss = {loss:.4f}, Tweedie Val Loss = {val_loss:.4f}")

# -------------------------------
# Evaluation
# -------------------------------
model.eval()
with torch.no_grad():
    pred = model(data)[1].cpu().numpy()  # Final predictions
    embed = model(data)[0].cpu().numpy()  # Node embeddings

# Save model summary to file
with open("./Code/Belgium_example/nn.txt", "a") as f:
    print(summary(model, data), file=f)

# Compute weighted averages of predictions and actual loss costs
pred_weighted_avg = np.average(pred.flatten()[test_idx], weights=data.exposure.detach().numpy()[test_idx])
actual_weighted_avg = np.average(data.y.detach().numpy()[test_idx], weights=data.exposure.detach().numpy()[test_idx])
print(f"Predicted Weighted Average: {pred_weighted_avg:.4f}")
print(f"Actual Weighted Average: {actual_weighted_avg:.4f}")

# Plot training and validation loss
plt.figure(figsize=(10, 6))
plt.plot(losses, label="Training Loss")
plt.plot(val_losses, label="Validation Loss")
plt.yscale('log', base=10)
plt.legend()
plt.show()

# Add predicted loss cost to GeoDataFrame and plot
pc_claims_gpd['lc_gnn'] = pred
pc_claims_gpd.plot(column='loss_cost')
plt.show()
pc_claims_gpd.plot(column='lc_gnn')
plt.show()

# -------------------------------
# Node embeddings (t-SNE)
# -------------------------------
embed_df = pd.DataFrame(embed)  # Raw node embeddings
embed_df.rename('X{}'.format, axis=1, inplace=True)  # Rename columns (X0, X1, ...)

# t-SNE for dimensionality reduction of embeddings
embed_tsne = TSNE(n_components=8, method='exact').fit_transform(embed)
embed_tsne_df = pd.DataFrame(embed_tsne)
embed_tsne_df.rename('V{}'.format, axis=1, inplace=True)

# Combine original GeoDataFrame, embeddings, and t-SNE features
final_df = pd.concat([pc_claims_gpd, embed_df, embed_tsne_df], axis=1)
final_df.to_file('./Data/Belgium/Belgium_GNN.shp')  # Save to shapefile

# -------------------------------
# Permutation feature importance
# -------------------------------
model.eval()
with torch.no_grad():
    _, baseline_pred = model(data)  # Baseline prediction without permutation

# Compute baseline Tweedie loss on test set
baseline_loss = tweedie_loss(baseline_pred[data.test_mask], data.y[data.test_mask], data.exposure[data.test_mask])

# Node feature importance via permutation
node_feature_importance = []
for i in range(data.num_node_features):
    x_permuted = data.x.clone()
    x_permuted[:, i] = x_permuted[:, i][torch.randperm(x_permuted.size(0))]  # Shuffle one feature
    data_perm = copy.deepcopy(data)
    data_perm.x = x_permuted
    with torch.no_grad():
        _, pred_perm = model(data_perm)
    perm_loss = tweedie_loss(pred_perm[data.test_mask], data.y[data.test_mask], data.exposure[data.test_mask])
    node_feature_importance.append((perm_loss.item() - baseline_loss.item()))  # Loss increase

# Edge feature importance via permutation
edge_feature_importance = []
for i in range(data.edge_attr.shape[1]):
    edge_attr_permuted = data.edge_attr.clone()
    edge_attr_permuted[:, i] = edge_attr_permuted[:, i][torch.randperm(edge_attr_permuted.size(0))]  # Shuffle
    data_perm = copy.deepcopy(data)
    data_perm.edge_attr = edge_attr_permuted
    with torch.no_grad():
        _, pred_perm = model(data_perm)
    perm_loss = tweedie_loss(pred_perm[data.test_mask], data.y[data.test_mask], data.exposure[data.test_mask])
    edge_feature_importance.append((perm_loss.item() - baseline_loss.item()))  # Loss increase

# -------------------------------
# Plot feature importance
# -------------------------------
# Define node and edge feature names for plotting
node_feature_names = ['deg', 'clo', 'bet', 'eig', 'pagerank', 'centr_x', 'centr_y', 'area', 'perimeter'] + \
    [f'cluster_{i}' for i in range(num_clusters)]
edge_feature_names = ['border_length', 'centroid_distance', 'edge_centrality']

node_feature_importance = np.abs(node_feature_importance) / sum(np.abs(node_feature_importance))
edge_feature_importance = np.abs(edge_feature_importance) / sum(np.abs(edge_feature_importance))

node_feats_imp = pd.DataFrame({"name" : node_feature_names, "value" : node_feature_importance})
edge_feats_imp = pd.DataFrame({"name" : edge_feature_names, "value" : edge_feature_importance})

node_feats_imp.to_csv('./Data/Belgium/node_feats_imp.csv')
edge_feats_imp.to_csv('./Data/Belgium/edge_feats_imp.csv')

plt.figure(figsize=(14,5))

# Node feature importance bar plot
plt.subplot(1,2,1)
plt.bar(range(len(node_feature_importance)), node_feature_importance)
plt.xticks(range(len(node_feature_importance)), node_feature_names, rotation=45, ha='right')
plt.ylabel("Increase in Tweedie Loss")
plt.title("Node Feature Importance")

# Edge feature importance bar plot
plt.subplot(1,2,2)
plt.bar(range(len(edge_feature_importance)), edge_feature_importance, color='orange')
plt.xticks(range(len(edge_feature_importance)), edge_feature_names, rotation=45, ha='right')
plt.ylabel("Increase in Tweedie Loss")
plt.title("Edge Feature Importance")

plt.tight_layout()
plt.show()

# 1. Load Data
#    ├─ Shapefile of Belgium postal codes (geometries)
#    └─ Claims CSV (exposure & amount)

# 2. Aggregate Claims
#    └─ Compute total exposure, amount, and loss cost per postal code

# 3. Merge Data
#    └─ Combine claims with shapefile → GeoDataFrame

# 4. Spatial Graph Construction
#    ├─ Project geometries to local CRS
#    ├─ Use spatial index to find neighbors
#    ├─ Compute border lengths & adjacency
#    └─ Create NetworkX graph with nodes=postal codes, edges=shared borders

# 5. Node Feature Engineering
#    ├─ Centrality features (degree, closeness, betweenness, eigenvector, PageRank)
#    ├─ Centroid coordinates (x, y)
#    ├─ Area & perimeter (normalized)
#    └─ Community clusters (one-hot encoding)

# 6. Edge Feature Engineering
#    ├─ Border length
#    ├─ Euclidean distance between centroids
#    └─ Edge betweenness centrality

# 7. Prepare Training/Validation/Test Splits
#    ├─ Mask nodes with exposure
#    ├─ Visualize subgraphs for each split
#    └─ Create boolean masks for PyG

# 8. Convert to PyG Dataset
#    ├─ Assign node features (x) and edge features (edge_attr)
#    ├─ Set target labels (loss_cost)
#    └─ Attach exposure, train/val/test masks

# 9. Define Tweedie Loss
#    └─ Custom loss suitable for insurance claims

# 10. Define Graph Neural Network (NNConvModel)
#     ├─ Edge neural networks for message passing
#     ├─ NNConv layers with batch norm, leaky ReLU, dropout, residuals
#     └─ Fully connected layers → prediction

# 11. Training Loop
#     ├─ Forward pass
#     ├─ Compute Tweedie loss
#     ├─ Backprop + optimizer step
#     ├─ Learning rate scheduler
#     └─ Early stopping based on validation loss

# 12. Evaluation
#     ├─ Make predictions
#     ├─ Compute weighted average predictions
#     ├─ Plot training & validation loss
#     └─ Visualize predicted vs actual loss cost on map

# 13. Node Embeddings & t-SNE
#     ├─ Extract embeddings from final NNConv layer
#     ├─ Apply t-SNE for dimensionality reduction
#     └─ Save GeoDataFrame with embeddings

# 14. Permutation Feature Importance
#     ├─ Shuffle each node feature → compute increase in loss
#     ├─ Shuffle each edge feature → compute increase in loss
#     └─ Visualize importance via bar plots
