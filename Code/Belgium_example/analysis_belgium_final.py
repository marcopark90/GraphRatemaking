import torch
from torch.nn import Linear
import networkx as nx
import igraph as ig
import matplotlib.pyplot as plt
import torch.nn.functional as F
from torch_geometric.nn import GCNConv, summary
from torch_geometric.nn.norm import BatchNorm
from torch_geometric.utils import from_networkx
from itertools import compress, combinations
import numpy as np
import geopandas as gpd
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.manifold import TSNE

import os
os.chdir('/home/marco/Documents/gitrepos/Graph_ratemaking')

# Load Data
pc_map = gpd.read_file('./Data/Belgium/Belgium.shp')
claims = pd.read_csv("./Data/Belgium/claims_belgium.csv")

claims_geo = gpd.GeoDataFrame(claims, geometry=gpd.points_from_xy(
    claims.long, claims.lat), crs='WGS84')
full_map_geo = pc_map.sjoin(claims_geo, how='left', predicate='contains')

full_map_geo = full_map_geo.groupby('geometry', as_index=False)\
    .apply(lambda x: pd.Series({
        'exposure': np.nansum(x.exposure),
        'amount': np.nansum(x.amount),
        'nclaims': np.nansum(x.nclaims)
    }), include_groups=False)\
    .assign(loss_cost=lambda x: np.where(x.exposure == 0, 0, x.amount / x.exposure))

full_map_geo = gpd.GeoDataFrame(
    full_map_geo, geometry=full_map_geo.geometry, crs='WGS84')

# Create Graph
adj_dict = {i: np.where([full_map_geo.geometry[i].touches(j) for j in full_map_geo.geometry])[
    0] for i in np.arange(0, len(full_map_geo.geometry))}
graph = nx.Graph(adj_dict)

graph_ig = ig.Graph.from_networkx(graph)
dendrogram = graph_ig.community_fastgreedy()
clusters = dendrogram.as_clustering()
cluster_vector = np.array([cluster_id for node in range(len(
    graph_ig.vs)) for cluster_id, cluster in enumerate(clusters) if node in cluster])

for i in np.unique(cluster_vector):
    graph.add_edges_from(combinations(np.where(cluster_vector == i)[0], 2))

centr = full_map_geo.centroid.get_coordinates().to_dict('index')
pos = {i: np.array([centr[i]['x'], centr[i]['y']])
       for i in np.arange(0, len(pc_map.geometry))}
nx.draw(graph, pos=pos, node_size=15, node_color=cluster_vector)
plt.show()

# Assign Node Attributes
areas = full_map_geo.geometry.to_crs(3857).area / 1e6
pers = full_map_geo.geometry.to_crs(3857).length / 1e3
attr = pd.DataFrame({'areas': areas, 'pers': pers})
nx.set_node_attributes(graph, attr.to_dict('index'))

# Split Data
all_idx = np.where(full_map_geo.exposure != 0)[0]
train_idx, test_idx = train_test_split(all_idx, test_size=0.3, train_size=0.7)
train_idx, val_idx = train_test_split(train_idx, test_size=0.3, train_size=0.7)

train_mask = torch.tensor(
    [i in train_idx for i in range(len(full_map_geo))], dtype=torch.bool)
test_mask = torch.tensor(
    [i in test_idx for i in range(len(full_map_geo))], dtype=torch.bool)
val_mask = torch.tensor(
    [i in val_idx for i in range(len(full_map_geo))], dtype=torch.bool)

# Convert to PyG Dataset
dataset = from_networkx(graph, group_node_attrs=['areas', 'pers'])
dataset.y = torch.tensor(full_map_geo.loss_cost.values, dtype=torch.float)
dataset.exposure = torch.tensor(
    full_map_geo.exposure.values, dtype=torch.float)
dataset.train_mask, dataset.test_mask, dataset.val_mask = train_mask, test_mask, val_mask

# Tweedie Loss Function (Optimized)
def tweedie_loss(preds, targets, weights, power=1.5):
    preds = torch.clamp(preds, min=1e-6)  # Prevent log(0) issues
    targets = torch.clamp(targets, min=0)
    term_1 = torch.pow(targets, 2 - power) / ((1 - power) * (2 - power))
    term_2 = targets * torch.pow(preds, 1 - power) / (1 - power)
    term_3 = torch.pow(preds, 2 - power) / (2 - power)
    return torch.sum(weights * (2 * (term_1 - term_2 + term_3))) / torch.sum(weights)

# Optimized GCN Model
class GCN(torch.nn.Module):
    def __init__(self, input_dim):
        super().__init__()
        self.conv1 = GCNConv(input_dim, 32, improved=True)
        self.conv2 = GCNConv(32, 64, improved=True)
        self.conv3 = GCNConv(64, 128, improved=True)

        self.batch_norm1 = BatchNorm(32)
        self.batch_norm2 = BatchNorm(64)
        self.batch_norm3 = BatchNorm(128)

        self.fnn1 = Linear(128, 64)
        self.fnn2 = Linear(64, 32)
        self.fnn3 = Linear(32, 1)

        torch.nn.init.xavier_uniform_(self.conv1.lin.weight)
        torch.nn.init.xavier_uniform_(self.conv2.lin.weight)
        torch.nn.init.xavier_uniform_(self.conv3.lin.weight)

        torch.nn.init.xavier_uniform_(self.fnn1.weight)
        torch.nn.init.xavier_uniform_(self.fnn2.weight)
        torch.nn.init.xavier_uniform_(self.fnn3.weight)

    def forward(self, data):
        x, edge_index = data.x, data.edge_index

        x = self.conv1(x, edge_index)
        x = self.batch_norm1(x)
        x = F.leaky_relu(x, negative_slope=0.01)
        x = F.dropout(x, p=0.1, training=self.training)

        x = self.conv2(x, edge_index)
        x = self.batch_norm2(x)
        x = F.leaky_relu(x, negative_slope=0.01)
        x = F.dropout(x, p=0.1, training=self.training)

        x = self.conv3(x, edge_index)
        x = self.batch_norm3(x)
        x = F.leaky_relu(x, negative_slope=0.01)
        x = F.dropout(x, p=0.1, training=self.training)

        y = x  # Embeddings

        x = self.fnn1(x)
        x = F.leaky_relu(x, negative_slope=0.01)
        x = F.dropout(x, p=0.1, training=self.training)

        x = self.fnn2(x)
        x = F.leaky_relu(x, negative_slope=0.01)
        x = F.dropout(x, p=0.1, training=self.training)

        x = self.fnn3(x)  # Remove ReLU to allow negative values
        # x = F.relu(x)

        return y, x

# Training
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
data = dataset.to(device)
input_dim = dataset.num_node_features
model = GCN(input_dim).to(device)

optimizer = torch.optim.AdamW(model.parameters(), lr=0.005, weight_decay=1e-4)
scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
    optimizer, mode='min', factor=0.5, patience=20)

train_mask, val_mask = data.train_mask.to(device), data.val_mask.to(device)

losses, val_losses = [], []

best_val_loss = float('inf')
patience = 10
counter = 0

for epoch in range(500):
    print(epoch)
    model.train()

    optimizer.zero_grad()
    _, out = model(data)

    loss = tweedie_loss(
        out[train_mask], data.y[train_mask], data.exposure[train_mask])
    val_loss = tweedie_loss(
        out[val_mask], data.y[val_mask], data.exposure[val_mask])

    losses.append(loss.item())
    val_losses.append(val_loss.item())

    loss.backward()
    optimizer.step()
    scheduler.step(val_loss)

    model.eval()
    with torch.no_grad():
        _, out = model(data)
        val_loss = tweedie_loss(
            out[val_mask], data.y[val_mask], data.exposure[val_mask])

    # Early stopping logic
    if val_loss.item() < best_val_loss:
        best_val_loss = val_loss.item()
        counter = 0  # Reset counter if improvement
    else:
        counter += 1  # Increment counter if no improvement
        if counter >= patience:
            print("Early stopping triggered")
            break

    if epoch % 10 == 0:
        print(f"Epoch {epoch}: Loss = {loss:.4f}, Val Loss = {val_loss:.4f}")


# Evaluation
model.eval()
pred = model(data)[1].detach().cpu().numpy()
embed = model(data)[0].detach().cpu().numpy()

with open("./Code/Belgium_example/nn.txt", "a") as f:
    print(summary(model, data), file=f)

# Weighted Averages
pred_weighted_avg = np.average(
    pred.flatten(), weights=data.exposure.detach().numpy())
actual_weighted_avg = np.average(
    data.y.detach().numpy(), weights=data.exposure.detach().numpy())

print(f"Predicted Weighted Average: {pred_weighted_avg:.4f}")
print(f"Actual Weighted Average: {actual_weighted_avg:.4f}")

# Loss Plot
plt.plot(losses, label="Training Loss")
plt.plot(val_losses, label="Validation Loss")
plt.yscale('log', base=10)
plt.legend()
plt.show()

df = full_map_geo.assign(lcost_plot=np.where(
    full_map_geo['exposure'] == 0, np.nan, full_map_geo['loss_cost']))
df['lc_gnn'] = pred

df.plot(column='lcost_plot')
plt.show()

df.plot(column='lc_gnn')
plt.show()

embed_df = pd.DataFrame(embed)
embed_df.rename('X{}'.format, axis=1, inplace=True)

embed_tsne = TSNE(n_components=8, method='exact').fit_transform(embed)
embed_tsne_df = pd.DataFrame(embed_tsne)
embed_tsne_df.rename('V{}'.format, axis=1, inplace=True)

df.drop(columns='lcost_plot', inplace=True)
final_df = pd.concat([df, embed_df, embed_tsne_df], axis=1)

final_df = pd.concat(
    [df[['geometry', 'lc_gnn']], embed_df, embed_tsne_df], axis=1)

final_df.to_file('./Data/Belgium/Belgium_GNN.shp')
