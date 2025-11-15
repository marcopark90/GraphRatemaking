# ============================================================================
# Main Analysis Script: Applications of Graph Theory in Territorial Ratemaking
# ============================================================================
# This script orchestrates the complete analysis pipeline for comparing
# traditional territorial ratemaking methods with graph-based approaches.
# The analysis includes:
#   - Data loading and preprocessing
#   - Graph construction from spatial adjacency
#   - Traditional GAM models (categorical, geographic, graph features)
#   - Graph Neural Network (GNN) predictions
#   - Model evaluation and comparison
#   - Visualization of results
# ============================================================================

# Load required libraries
# -----------------------
# Statistical modeling
library(mgcv)          # Generalized Additive Models (GAM)
library(tidymodels)    # Modeling framework
library(tweedie)       # Tweedie distribution for insurance claims
library(statmod)       # Statistical modeling utilities

# Data manipulation and visualization
library(tidyverse)     # Data manipulation (dplyr, ggplot2, etc.)
library(magrittr)      # Pipe operators
library(glue)          # String interpolation
library(scales)        # Scale functions for plots
library(viridis)       # Color scales for plots
library(patchwork)     # Combining plots
library(gt)            # Creating tables

# Spatial data handling
library(sf)            # Simple features for spatial data
library(gstat)         # Geostatistical analysis
library(spdep)         # Spatial dependence analysis
library(units)         # Unit conversions

# Graph analysis
library(igraph)        # Graph manipulation and analysis
library(ggnetwork)    # Network visualization with ggplot2

# Machine learning and dimensionality reduction
library(Rtsne)         # t-SNE for dimensionality reduction
library(fields)        # Spatial statistics
library(pracma)        # Practical numerical math functions

# Utilities
library(classInt)      # Class intervals for maps
library(ggforce)       # Additional ggplot2 extensions
library(CASdatasets)   # CAS insurance datasets
library(DescTools)     # Descriptive statistics tools

# Disable S2 geometry engine for compatibility with older spatial operations
sf_use_s2(FALSE)

# Set random seed for reproducibility
set.seed(999)

# Get device dimensions for consistent plot sizing
zoom_dims <- dev.size("in")

# Source analysis scripts in order
# ---------------------------------
# 1. Function definitions: Custom functions for metrics, plotting, etc.
source("./Code/Belgium_example/1_function_definitions.R")

# 2. Data manipulation: Load and preprocess data, create train/test splits
source("./Code/Belgium_example/2_data_manipulation.R")

# 3. Graphs: Create visualizations of data, graphs, and features
source("./Code/Belgium_example/3_graphs.R")

# 4. Modeling: Fit GAM models (categorical, geographic, graph, embedding)
source("./Code/Belgium_example/4_modeling.R")

# 5. Predictions: Evaluate models and compute comprehensive metrics
source("./Code/Belgium_example/5_predictions.R")

# 6. Prediction maps: Visualize model predictions on maps
source("./Code/Belgium_example/7_pred_maps.R")
