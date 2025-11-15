# GraphRatemaking

**Repository for the paper: Applications of Graph Theory in Territorial Ratemaking**

This repository contains code and data for comparing traditional territorial ratemaking methods with graph-based approaches, including Graph Neural Networks (GNNs), for insurance pricing.

## Overview

Territorial ratemaking is a fundamental task in insurance pricing where insurers need to determine appropriate premiums based on geographic location. Traditional methods rely on administrative boundaries (e.g., provinces, postal codes) or spatial smoothing techniques. This project explores how graph theory and graph neural networks can improve territorial ratemaking by:

1. **Capturing spatial relationships** through graph structures based on geographic adjacency
2. **Learning complex spatial patterns** using graph neural networks
3. **Improving risk discrimination** compared to traditional methods

## Repository Structure

```
GraphRatemaking/
├── Code/
│   └── Belgium_example/
│       ├── 0_full_analysis.R          # Main analysis script (orchestrates all steps)
│       ├── 1_function_definitions.R   # Custom functions for metrics and visualization
│       ├── 2_data_manipulation.R      # Data loading and preprocessing
│       ├── 3_graphs.R                  # Visualization generation
│       ├── 4_modeling.R                # GAM model fitting
│       ├── 5_predictions.R             # Model evaluation and metrics
│       ├── 6_final_smoothing.R        # Spatial effect visualization
│       ├── 7_pred_maps.R               # Prediction map generation
│       └── analysis_belgium_final.py  # Python GNN implementation
├── Data/
│   └── Belgium/
│       ├── Belgium.shp                 # Belgian postal code shapefile
│       ├── Belgium_GNN.shp              # Shapefile with GNN features and predictions
│       └── claims_belgium.csv          # Motor insurance claims data
└── Graphs/                             # Generated visualizations
```

## Methodology

### Data

The analysis uses the **Belgian Motor Third Party Liability (MTPL)** dataset from 1997, which includes:
- Claims information (amount, frequency)
- Policy characteristics (coverage, fuel type, vehicle age, etc.)
- Geographic location (postal codes, coordinates)
- Exposure (policy years)

### Graph Construction

A spatial graph is constructed where:
- **Nodes** = Belgian postal codes
- **Edges** = Shared borders between adjacent postal codes
- **Node features** = Centrality measures (degree, closeness, betweenness, eigenvector, PageRank), spatial attributes (centroid coordinates, area, perimeter), and community clusters
- **Edge features** = Border length, centroid distance, edge betweenness centrality

### Models Compared

1. **Categorical Model (Province-based)**
   - Traditional territorial ratemaking using Belgian provinces
   - Simple but interpretable

2. **Geographic Model (Spatial Smoothing)**
   - Uses thin-plate spline smoothing over longitude/latitude
   - Captures smooth spatial patterns without administrative boundaries

3. **Graph Model (Centrality Features)**
   - Uses graph centrality features in a GAM framework
   - Leverages graph structure without deep learning

4. **Embedding Model (t-SNE Features)**
   - Uses t-SNE reduced embeddings from the GNN model
   - Captures learned spatial patterns from graph neural network

5. **Graph Neural Network (GNN)**
   - Deep learning model using PyTorch Geometric
   - NNConv layers with edge features
   - Directly learns from graph structure and spatial relationships

### Evaluation Metrics

Models are evaluated using comprehensive metrics:

- **Accuracy**: RMSE, MAE, SMAPE
- **Distribution**: Jensen-Shannon divergence
- **Discrimination**: Gini coefficient
- **Calibration**: Loss ratio, calibration error
- **Quantile Performance**: Loss at 50th, 75th, 90th, 95th percentiles
- **Spatial Dependence**: Moran's I test for residual spatial autocorrelation

## Getting Started

### Prerequisites

#### R Packages
```r
install.packages(c(
  "mgcv", "tidyverse", "tidymodels", "sf", "igraph", 
  "ggnetwork", "viridis", "patchwork", "Rtsne", 
  "CASdatasets", "spdep", "yardstick", "DescTools"
))
```

#### Python Packages
```bash
pip install torch torch-geometric networkx geopandas pandas numpy scikit-learn matplotlib
```

### Running the Analysis

#### Step 1: Run Python GNN Script
```bash
cd Code/Belgium_example
python analysis_belgium_final.py
```

This script:
- Loads spatial and claims data
- Constructs the spatial graph
- Computes graph features (centrality, communities)
- Trains the Graph Neural Network
- Generates predictions and node embeddings
- Saves results to `Data/Belgium/Belgium_GNN.shp`

#### Step 2: Run R Analysis
```r
# In R or RStudio
source("Code/Belgium_example/0_full_analysis.R")
```

This will:
- Load and preprocess data
- Fit traditional GAM models
- Evaluate all models
- Generate visualizations
- Save plots to `Graphs/` directory

## Key Features

### Graph Neural Network Architecture

The GNN model (`analysis_belgium_final.py`) uses:
- **NNConv layers**: Neural network-based message passing with edge features
- **Batch normalization**: Stabilizes training
- **Residual connections**: Helps with deep network training
- **Tweedie loss**: Appropriate for insurance claims data
- **Early stopping**: Prevents overfitting

### Custom Evaluation Metrics

The R code includes custom metrics for insurance model evaluation:
- **Jensen-Shannon Divergence**: Measures distributional similarity
- **Gini Coefficient**: Measures risk discrimination ability
- **Quantile Loss**: Evaluates performance at different risk levels
- **Calibration Error**: Measures pricing accuracy

### Visualization

The analysis generates comprehensive visualizations:
- Feature importance plots (node and edge features)
- Loss cost maps (actual and predicted)
- Graph visualizations (clusters, train/test splits)
- t-SNE embeddings of GNN node representations
- Model comparison plots (quantile loss, claim size performance)

## Results

The analysis compares all five models across multiple dimensions:
- Overall accuracy (RMSE, MAE, SMAPE)
- Risk discrimination (Gini coefficient)
- Distributional alignment (Jensen-Shannon divergence)
- Calibration (loss ratio, calibration error)
- Tail risk prediction (quantile loss at high percentiles)
- Spatial pattern capture (Moran's I on residuals)

## File Descriptions

### R Scripts

- **`0_full_analysis.R`**: Main orchestrator script that sources all other scripts in order
- **`1_function_definitions.R`**: Custom functions for actuarial metrics, plotting, and evaluation metrics
- **`2_data_manipulation.R`**: Data loading, preprocessing, train/test splits, t-SNE embeddings
- **`3_graphs.R`**: Generates all visualization plots
- **`4_modeling.R`**: Fits four GAM models (categorical, geographic, graph, embedding)
- **`5_predictions.R`**: Comprehensive model evaluation with multiple metrics
- **`6_final_smoothing.R`**: Extracts and visualizes spatial effects from models
- **`7_pred_maps.R`**: Creates prediction maps for all models

### Python Script

- **`analysis_belgium_final.py`**: Complete GNN implementation including:
  - Graph construction from spatial adjacency
  - Feature engineering (centrality, communities, spatial attributes)
  - GNN model definition and training
  - Prediction generation
  - Feature importance analysis
  - Node embedding extraction
