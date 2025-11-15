# ============================================================================
# Modeling: Generalized Additive Models (GAM)
# ============================================================================
# Fit four different GAM models to compare approaches:
#   1. Categorical: Province-based (traditional territorial ratemaking)
#   2. Geographic: Spatial smoothing using longitude/latitude
#   3. Graph: Graph centrality features (degree, eigenvector, etc.)
#   4. Embedding: t-SNE reduced embeddings from GNN
#
# All models use Tweedie distribution (p=1.5) which is standard for
# insurance claims data, and are weighted by exposure.
# ============================================================================

# Model 1: Categorical (Province-based)
# Traditional territorial ratemaking using administrative boundaries
cat_model <- gam(
  loss_cost ~ province,
  weights = exposure,
  family = Tweedie(p = 1.5, link = "log"),
  data = train_data
)

# Model 2: Geographic (Spatial Smoothing)
# Uses thin-plate spline smoothing over longitude/latitude coordinates
# Captures spatial patterns without administrative boundaries
geo_model <- gam(
  loss_cost ~ s(long, lat, bs = "tp"),
  weights = exposure,
  family = Tweedie(p = 1.5, link = "log"),
  data = train_data
)

# Model 3: Graph-based
# Uses graph centrality features computed from spatial adjacency graph
# Features: degree, eigenvector, closeness, betweenness centrality, PageRank, cluster
graph_model <- gam(
  loss_cost ~
    deg +
      eig +
      clo +
      bet +
      pagerank +
      cluster,
  weights = exposure,
  family = Tweedie(p = 1.5, link = "log"),
  data = train_data
)

# Model 4: Embedding-based
# Uses t-SNE reduced embeddings (V0-V7) from the GNN model
# These embeddings capture learned spatial patterns from the graph neural network
embed_model <- gam(
  loss_cost ~
    V0 +
      V1 +
      V2 +
      V3 +
      V4 +
      V5 +
      V6 +
      V7,
  weights = exposure,
  family = Tweedie(p = 1.5, link = "log"),
  data = train_data
)
