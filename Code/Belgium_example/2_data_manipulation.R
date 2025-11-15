# ============================================================================
# Data Import and Manipulation
# ============================================================================
# This script handles:
#   - Loading the Belgian MTPL (Motor Third Party Liability) dataset
#   - Creating province assignments from postal codes
#   - Loading spatial data (shapefiles)
#   - Merging claims data with spatial geometries
#   - Creating train/validation/test splits
#   - Computing t-SNE embeddings for visualization
# ============================================================================

# Load Belgian MTPL dataset from CASdatasets package
# This dataset contains motor insurance claims from Belgium in 1997
data(beMTPL97)

# Process claims data: convert to tibble and assign provinces based on postal codes
# Belgian provinces are determined by postal code ranges
claims <- beMTPL97 %>%
  as_tibble() %>%
  mutate(
    # Assign province based on postal code ranges
    province = case_when(
      postcode >= 2000 & postcode <= 2999 ~ "Antwerp",
      postcode >= 9000 & postcode <= 9999 ~ "East Flanders",
      (postcode >= 1500 & postcode <= 1999) |
        (postcode >= 3000 & postcode <= 3499) ~
        "Flemish Brabant",
      postcode >= 3500 & postcode <= 3999 ~ "Limburg",
      postcode >= 8000 & postcode <= 8999 ~ "West Flanders",
      postcode >= 7000 & postcode <= 7999 ~ "Hainaut",
      postcode >= 4000 & postcode <= 4999 ~ "Liège",
      postcode >= 6000 & postcode <= 6999 ~ "Luxembourg",
      postcode >= 5000 & postcode <= 5999 ~ "Namur",
      postcode >= 1300 & postcode <= 1499 ~ "Walloon Brabant",
      postcode >= 1000 & postcode <= 1299 ~ "Brussels",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(exposure = expo, pc = postcode) %>%
  select(
    id,
    nclaims,
    amount,
    exposure,
    coverage,
    fuel,
    use,
    fleet,
    sex,
    ageph,
    bm,
    agec,
    power,
    pc,
    province,
    long,
    lat
  )

# Save processed claims data
write_csv(claims, "./Data/Belgium/claims_belgium.csv")

# Load spatial data
# pc_map: Original postal code boundaries
# gnn_map: Postal codes with GNN predictions and features (from Python script)
pc_map <- st_read("./Data/Belgium/Belgium.shp")
gnn_map <- st_read("./Data/Belgium/Belgium_GNN.shp")

# Merge GNN map with original postal code data and convert cluster to factor
gnn_map <- gnn_map %>%
  left_join(pc_map %>% st_drop_geometry()) %>%
  mutate(cluster = factor(cluster))

# claims_agg <- claims %>%
#   group_by(pc) %>%
#   summarise(amount = sum(amount), exposure = sum(exposure)) %>%
#   mutate(loss_cost = amount / exposure) %>%
#   left_join(pc_map, by = join_by(pc == POSTCODE))
#
# pc_map <- pc_map %>%
#   left_join(gnn_map) %>%
#   left_join(
#     claims_agg %>% select(pc, amount, exposure, loss_cost),
#     by = join_by(POSTCODE == pc)
#   ) %>%
#   mutate(
#     area = st_area(pc_map) %>% set_units(km^2) %>% as.numeric(),
#     per = st_length(pc_map) %>% set_units(km) %>% as.numeric()
#   )

# Create hexagonal grid for alternative spatial aggregation/visualization
hex_map <- st_make_grid(pc_map, cellsize = .05, square = FALSE) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_join(pc_map, join = st_intersects, left = FALSE)

# Create graph from spatial adjacency (postal codes that touch each other)
# This graph structure is used for spatial analysis and visualization
graph_data <- graph_from_adj_list(st_touches(gnn_map), mode = "all")

# Extract indices for different data splits (defined in Python GNN script)
all_idx <- which(gnn_map$data_type != "no_exp")      # All postal codes with exposure
train_idx <- which(gnn_map$data_type == "train")     # Training set indices
val_idx <- which(gnn_map$data_type == "val")         # Validation set indices
test_idx <- which(gnn_map$data_type == "test")       # Test set indices

# pc_map <- pc_map %>%
#   mutate(
#     deg_cent = centr_degree(graph_data)$res,
#     eig_cent = centr_eigen(graph_data)$vector,
#     clos_cent = centr_clo(graph_data)$res,
#     bet_cent = centr_betw(graph_data)$res,
#     cluster = as.factor(membership(cl))
#   )

# ============================================================================
# Train/Test Split
# ============================================================================
# Create data splits for model training and evaluation
# The splits were already defined in the Python GNN script, so we use those
# ============================================================================

# modeling_data <- claims %>%
#   left_join(
#     gnn_map %>%
#       st_drop_geometry() %>%
#       select(
#         POSTCODE,
#         lc_gnn,
#         starts_with("V"),
#         cluster,
#         deg,
#         clo,
#         bet,
#         eig,
#         pagerank
#       ),
#     by = join_by(pc == POSTCODE)
#   )
#
# modeling_data %>% glimpse()
#
# ind_data_split <- initial_split(modeling_data, strata = amount)
# ind_train_data <- training(ind_data_split)
# ind_test_data <- testing(ind_data_split)

# Create tidymodels split object combining train and validation for training,
# and test set for assessment. Exclude raw node embeddings (X0, X1, ...)
# as we'll use t-SNE reduced embeddings (V0, V1, ...) instead
data_split <- make_splits(
  x = gnn_map %>%
    st_drop_geometry() %>%
    filter(data_type == "train" | data_type == "val") %>%
    select(-starts_with("X")),  # Exclude raw embeddings
  assessment = gnn_map %>%
    st_drop_geometry() %>%
    filter(data_type == "test") %>%
    select(-starts_with("X"))    # Exclude raw embeddings
)

# Extract training and test datasets
train_data <- training(data_split)
test_data <- testing(data_split)

# ============================================================================
# t-SNE Embedding and Clustering
# ============================================================================
# Reduce high-dimensional GNN node embeddings to 2D using t-SNE for visualization
# Then perform k-means clustering on the 2D embeddings
# ============================================================================

# Extract raw node embeddings (from GNN model) for dimensionality reduction
embeddings <- gnn_map %>%
  st_drop_geometry() %>%
  select(cluster, starts_with("X"))

# Apply t-SNE to reduce embeddings to 2D for visualization
# t-SNE preserves local neighborhood structure in the embedding space
tsne_embeddings <- Rtsne(
  embeddings %>% select(cluster, starts_with("X")) %>% unique(),
  dims = 2
) %>%
  use_series(Y)

# Perform k-means clustering on t-SNE embeddings (11 clusters to match graph clusters)
k_clusters <- kmeans(tsne_embeddings, 11)

# Extract cluster centers for visualization
centers <- k_clusters %>%
  use_series(centers) %>%
  as_tibble(.name_repair = ~ c("center_x", "center_y")) %>%
  mutate(cluster = factor(1:11))

# Calculate maximum distance from each cluster center to its farthest point
# This is used to draw circles around clusters in t-SNE plots
cent_dist <- tsne_embeddings %>%
  as_tibble(.name_repair = ~ c("V1", "V2")) %>%
  mutate(cluster = k_clusters %>% use_series(cluster) %>% as_factor()) %>%
  left_join(centers) %>%
  mutate(distance = sqrt((V1 - center_x)^2 + (V2 - center_y)^2)) %>%
  group_by(cluster) %>%
  slice_max(distance, with_ties = FALSE) %>%
  select(center_x, center_y, distance)
