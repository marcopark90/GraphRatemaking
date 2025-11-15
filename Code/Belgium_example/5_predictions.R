# ============================================================================
# Model Evaluation and Predictions
# ============================================================================
# This script evaluates all models on the test set using comprehensive metrics:
#   - Accuracy metrics: RMSE, MAE, SMAPE
#   - Distribution metrics: Jensen-Shannon divergence
#   - Discrimination: Gini coefficient
#   - Calibration: Loss ratio, calibration error
#   - Quantile performance: Loss at different quantiles (50th, 75th, 90th, 95th)
#   - Spatial dependence: Moran's I test for residual spatial autocorrelation
# ============================================================================

# Set display precision for numeric output
options(pillar.sigfig = 7)

# Generate predictions from all models on test set
# Note: GNN predictions (lc_gnn) come directly from the Python script
pred_df <- test_data %>%
  mutate(
    loss_cost = amount / exposure,  # Actual loss cost
    cat_pred = predict(cat_model, newdata = ., type = "response"),      # Province model
    geo_pred = predict(geo_model, newdata = ., type = "response"),     # Geographic model
    graph_pred = predict(graph_model, newdata = ., type = "response"), # Graph features model
    embed_pred = predict(embed_model, newdata = ., type = "response"), # Embedding model
    gnn_pred = lc_gnn  # GNN predictions (from Python script)
  )

# Define comprehensive metric set for model evaluation
# Includes accuracy, distribution, discrimination, and quantile metrics
comprehensive_metrics <- metric_set(
  rmse,
  mae,
  smape,
  js_divergence,
  gini_coef,
  quantile_loss_50,
  quantile_loss_75,
  quantile_loss_90,
  quantile_loss_95
)

results <- pred_df %>%
  select(
    loss_cost,
    cat_pred,
    geo_pred,
    graph_pred,
    embed_pred,
    gnn_pred,
    exposure
  ) %>%
  pivot_longer(-c(loss_cost, exposure)) %>%
  group_by(name) %>%
  comprehensive_metrics(loss_cost, value, case_weights = exposure) %>%
  group_by(.metric) %>%
  arrange(.estimate)

results <- pred_df %>%
  select(
    loss_cost,
    cat_pred,
    geo_pred,
    graph_pred,
    embed_pred,
    gnn_pred,
    exposure
  ) %>%
  pivot_longer(
    -c(loss_cost, exposure),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  group_by(model) %>%
  comprehensive_metrics(loss_cost, prediction, case_weights = exposure) %>%
  group_by(.metric) %>%
  arrange(.estimate)

summary_table <- results %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  select(
    model,
    rmse,
    mae,
    smape,
    js_divergence,
    gini,
    starts_with("quantile_loss")
  ) %>%
  arrange(rmse)

summary_table

cat_metrics <- summary_table %>% filter(model == "cat_pred")
cat_rmse <- cat_metrics$rmse
cat_mae <- cat_metrics$mae
cat_smape <- cat_metrics$smape
cat_js <- cat_metrics$js_divergence
cat_gini <- cat_metrics$gini
cat_q50 <- cat_metrics$quantile_loss_0.5
cat_q75 <- cat_metrics$quantile_loss_0.75
cat_q90 <- cat_metrics$quantile_loss_0.9
cat_q95 <- cat_metrics$quantile_loss_0.95

embed_metrics <- summary_table %>% filter(model == "embed_pred")
embed_rmse <- embed_metrics$rmse
embed_mae <- embed_metrics$mae
embed_smape <- embed_metrics$smape
embed_js <- embed_metrics$js_divergence
embed_gini <- embed_metrics$gini
embed_q50 <- embed_metrics$quantile_loss_0.5
embed_q75 <- embed_metrics$quantile_loss_0.75
embed_q90 <- embed_metrics$quantile_loss_0.9
embed_q95 <- embed_metrics$quantile_loss_0.95

graph_metrics <- summary_table %>% filter(model == "graph_pred")
graph_rmse <- graph_metrics$rmse
graph_mae <- graph_metrics$mae
graph_smape <- graph_metrics$smape
graph_js <- graph_metrics$js_divergence
graph_gini <- graph_metrics$gini
graph_q50 <- graph_metrics$quantile_loss_0.5
graph_q75 <- graph_metrics$quantile_loss_0.75
graph_q90 <- graph_metrics$quantile_loss_0.9
graph_q95 <- graph_metrics$quantile_loss_0.95

geo_metrics <- summary_table %>% filter(model == "geo_pred")
geo_rmse <- geo_metrics$rmse
geo_mae <- geo_metrics$mae
geo_smape <- geo_metrics$smape
geo_js <- geo_metrics$js_divergence
geo_gini <- geo_metrics$gini
geo_q50 <- geo_metrics$quantile_loss_0.5
geo_q75 <- geo_metrics$quantile_loss_0.75
geo_q90 <- geo_metrics$quantile_loss_0.9
geo_q95 <- geo_metrics$quantile_loss_0.95

gnn_metrics <- summary_table %>% filter(model == "gnn_pred")
gnn_rmse <- gnn_metrics$rmse
gnn_mae <- gnn_metrics$mae
gnn_smape <- gnn_metrics$smape
gnn_js <- gnn_metrics$js_divergence
gnn_gini <- gnn_metrics$gini
gnn_q50 <- gnn_metrics$quantile_loss_0.5
gnn_q75 <- gnn_metrics$quantile_loss_0.75
gnn_q90 <- gnn_metrics$quantile_loss_0.9
gnn_q95 <- gnn_metrics$quantile_loss_0.95


# ============================================================================
# Calibration Analysis
# ============================================================================
# Evaluate how well models are calibrated by comparing predicted vs actual
# loss cost across prediction deciles. Well-calibrated models should have
# mean_abs_calibration_error close to 0.
# ============================================================================
calibration_results <- pred_df %>%
  pivot_longer(
    cols = c(cat_pred, geo_pred, graph_pred, embed_pred, gnn_pred),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  group_by(model) %>%
  mutate(pred_decile = ntile(prediction, 10)) %>%
  group_by(model, pred_decile) %>%
  summarize(
    mean_predicted = weighted.mean(prediction, exposure),
    mean_actual = weighted.mean(loss_cost, exposure),
    ratio = mean_actual / mean_predicted,
    .groups = "drop"
  ) %>%
  group_by(model) %>%
  summarize(
    mean_abs_calibration_error = mean(abs(ratio - 1)),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_calibration_error)

embed_calib <- calibration_results %>%
  filter(model == "embed_pred") %>%
  pull(mean_abs_calibration_error)
geo_calib <- calibration_results %>%
  filter(model == "geo_pred") %>%
  pull(mean_abs_calibration_error)
cat_calib <- calibration_results %>%
  filter(model == "cat_pred") %>%
  pull(mean_abs_calibration_error)
graph_calib <- calibration_results %>%
  filter(model == "graph_pred") %>%
  pull(mean_abs_calibration_error)
gnn_calib <- calibration_results %>%
  filter(model == "gnn_pred") %>%
  pull(mean_abs_calibration_error)

calibration_results

# ============================================================================
# Loss Ratio Analysis
# ============================================================================
# Compute overall loss ratio (actual/predicted) for each model.
# Ideal loss ratio = 1.0. Values < 0.95 indicate over-pricing,
# values > 1.05 indicate under-pricing.
# ============================================================================
loss_ratios <- pred_df %>%
  summarize(
    cat_pred = weighted.mean(loss_cost / cat_pred, exposure),
    geo_pred = weighted.mean(loss_cost / geo_pred, exposure),
    graph_pred = weighted.mean(loss_cost / graph_pred, exposure),
    embed_pred = weighted.mean(loss_cost / embed_pred, exposure),
    gnn_pred = weighted.mean(loss_cost / gnn_pred, exposure)
  ) %>%
  pivot_longer(everything(), names_to = "model", values_to = "loss_ratio") %>%
  mutate(
    deviation_from_ideal = abs(loss_ratio - 1),
    status = case_when(
      loss_ratio < 0.95 ~ "Over-pricing",
      loss_ratio > 1.05 ~ "Under-pricing",
      TRUE ~ "Well-calibrated"
    )
  ) %>%
  arrange(deviation_from_ideal)

embed_lr <- loss_ratios %>% filter(model == "embed_pred") %>% pull(loss_ratio)
geo_lr <- loss_ratios %>% filter(model == "geo_pred") %>% pull(loss_ratio)
cat_lr <- loss_ratios %>% filter(model == "cat_pred") %>% pull(loss_ratio)
graph_lr <- loss_ratios %>% filter(model == "graph_pred") %>% pull(loss_ratio)
gnn_lr <- loss_ratios %>% filter(model == "gnn_pred") %>% pull(loss_ratio)

loss_ratios

# ============================================================================
# Performance by Claim Size
# ============================================================================
# Evaluate model performance across different claim size categories.
# Important for understanding where models perform well/poorly.
# ============================================================================
claim_size_results <- pred_df %>%
  mutate(
    claim_size = cut(
      loss_cost,
      breaks = c(0, 50, 100, 200, Inf),
      labels = c("small", "medium", "large", "very_large")
    )
  ) %>%
  pivot_longer(
    cols = c(cat_pred, geo_pred, graph_pred, embed_pred, gnn_pred),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  filter(!is.na(claim_size)) %>%
  group_by(model, claim_size) %>%
  summarize(
    smape = mean(
      200 * abs(loss_cost - prediction) / (abs(loss_cost) + abs(prediction)),
      na.rm = TRUE
    ),
    mae = weighted.mean(abs(loss_cost - prediction), exposure, na.rm = TRUE),
    .groups = "drop"
  )

cat_small_smape <- claim_size_results %>%
  filter(model == "cat_pred", claim_size == "small") %>%
  pull(smape)
cat_medium_smape <- claim_size_results %>%
  filter(model == "cat_pred", claim_size == "medium") %>%
  pull(smape)
cat_large_smape <- claim_size_results %>%
  filter(model == "cat_pred", claim_size == "large") %>%
  pull(smape)
cat_very_large_smape <- claim_size_results %>%
  filter(model == "cat_pred", claim_size == "very_large") %>%
  pull(smape)
embed_small_smape <- claim_size_results %>%
  filter(model == "embed_pred", claim_size == "small") %>%
  pull(smape)
embed_medium_smape <- claim_size_results %>%
  filter(model == "embed_pred", claim_size == "medium") %>%
  pull(smape)
embed_large_smape <- claim_size_results %>%
  filter(model == "embed_pred", claim_size == "large") %>%
  pull(smape)
embed_very_large_smape <- claim_size_results %>%
  filter(model == "embed_pred", claim_size == "very_large") %>%
  pull(smape)
graph_small_smape <- claim_size_results %>%
  filter(model == "graph_pred", claim_size == "small") %>%
  pull(smape)
graph_medium_smape <- claim_size_results %>%
  filter(model == "graph_pred", claim_size == "medium") %>%
  pull(smape)
graph_large_smape <- claim_size_results %>%
  filter(model == "graph_pred", claim_size == "large") %>%
  pull(smape)
graph_very_large_smape <- claim_size_results %>%
  filter(model == "graph_pred", claim_size == "very_large") %>%
  pull(smape)
geo_small_smape <- claim_size_results %>%
  filter(model == "geo_pred", claim_size == "small") %>%
  pull(smape)
geo_medium_smape <- claim_size_results %>%
  filter(model == "geo_pred", claim_size == "medium") %>%
  pull(smape)
geo_large_smape <- claim_size_results %>%
  filter(model == "geo_pred", claim_size == "large") %>%
  pull(smape)
geo_very_large_smape <- claim_size_results %>%
  filter(model == "geo_pred", claim_size == "very_large") %>%
  pull(smape)
gnn_small_smape <- claim_size_results %>%
  filter(model == "gnn_pred", claim_size == "small") %>%
  pull(smape)
gnn_medium_smape <- claim_size_results %>%
  filter(model == "gnn_pred", claim_size == "medium") %>%
  pull(smape)
gnn_large_smape <- claim_size_results %>%
  filter(model == "gnn_pred", claim_size == "large") %>%
  pull(smape)
gnn_very_large_smape <- claim_size_results %>%
  filter(model == "gnn_pred", claim_size == "very_large") %>%
  pull(smape)

claim_size_results %>%
  select(model, claim_size, smape) %>%
  pivot_wider(names_from = claim_size, values_from = smape)


# ============================================================================
# Tail Risk Analysis
# ============================================================================
# Evaluate model performance on high-risk cases (top 10% of loss cost).
# Critical for insurance as tail risk drives profitability.
# ============================================================================
tail_results <- pred_df %>%
  filter(loss_cost >= quantile(loss_cost, 0.9)) %>%
  pivot_longer(
    cols = c(cat_pred, geo_pred, graph_pred, embed_pred, gnn_pred),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  group_by(model) %>%
  summarize(
    rmse = sqrt(weighted.mean((loss_cost - prediction)^2, exposure)),
    js_div = js_divergence_vec(loss_cost, prediction, case_weights = exposure),
    .groups = "drop"
  ) %>%
  arrange(rmse)

embed_tail_js <- tail_results %>% filter(model == "embed_pred") %>% pull(js_div)
gnn_tail_js <- tail_results %>% filter(model == "gnn_pred") %>% pull(js_div)
cat_tail_js <- tail_results %>% filter(model == "cat_pred") %>% pull(js_div)
embed_tail_js <- tail_results %>% filter(model == "embed_pred") %>% pull(js_div)
graph_tail_js <- tail_results %>% filter(model == "graph_pred") %>% pull(js_div)
geo_tail_js <- tail_results %>% filter(model == "geo_pred") %>% pull(js_div)
gnn_tail_js <- tail_results %>% filter(model == "gnn_pred") %>% pull(js_div)


metrics_table <- tibble(
  Metric = c(
    "Overall RMSE",
    "Overall MAE",
    "Overall SMAPE",
    "JS Divergence",
    "Gini (Discrimination)",
    "Calibration Error",
    "Loss Ratio",
    "Small Claims SMAPE",
    "Medium Claims SMAPE",
    "Large Claims SMAPE",
    "Very Large Claims SMAPE",
    "Tail JS Divergence",
    "Q50 (Median) Loss",
    "Q75 Loss",
    "Q90 Loss",
    "Q95 (Tail) Loss"
  ),
  Category = c(
    "Accuracy",
    "Accuracy",
    "Accuracy",
    "Distribution",
    "Discrimination",
    "Calibration",
    "Profitability",
    "Segmentation",
    "Segmentation",
    "Segmentation",
    "Segmentation",
    "Tail Risk",
    "Quantile",
    "Quantile",
    "Quantile",
    "Quantile"
  ),
  cat_pred = c(
    cat_rmse,
    cat_mae,
    cat_smape,
    cat_js,
    cat_gini,
    cat_calib,
    cat_lr,
    cat_small_smape,
    cat_medium_smape,
    cat_large_smape,
    cat_very_large_smape,
    cat_tail_js,
    cat_q50,
    cat_q75,
    cat_q90,
    cat_q95
  ),
  embed_pred = c(
    embed_rmse,
    embed_mae,
    embed_smape,
    embed_js,
    embed_gini,
    embed_calib,
    embed_lr,
    embed_small_smape,
    embed_medium_smape,
    embed_large_smape,
    embed_very_large_smape,
    embed_tail_js,
    embed_q50,
    embed_q75,
    embed_q90,
    embed_q95
  ),
  graph_pred = c(
    graph_rmse,
    graph_mae,
    graph_smape,
    graph_js,
    graph_gini,
    graph_calib,
    graph_lr,
    graph_small_smape,
    graph_medium_smape,
    graph_large_smape,
    graph_very_large_smape,
    graph_tail_js,
    graph_q50,
    graph_q75,
    graph_q90,
    graph_q95
  ),
  geo_pred = c(
    geo_rmse,
    geo_mae,
    geo_smape,
    geo_js,
    geo_gini,
    geo_calib,
    geo_lr,
    geo_small_smape,
    geo_medium_smape,
    geo_large_smape,
    geo_very_large_smape,
    geo_tail_js,
    geo_q50,
    geo_q75,
    geo_q90,
    geo_q95
  ),
  gnn_pred = c(
    gnn_rmse,
    gnn_mae,
    gnn_smape,
    gnn_js,
    gnn_gini,
    gnn_calib,
    gnn_lr,
    gnn_small_smape,
    gnn_medium_smape,
    gnn_large_smape,
    gnn_very_large_smape,
    gnn_tail_js,
    gnn_q50,
    gnn_q75,
    gnn_q90,
    gnn_q95
  )
)

lower_is_better <- c(
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE
)

comparison_table <- metrics_table %>%
  mutate(
    Winner = pmap_chr(
      list(
        cat_pred,
        embed_pred,
        graph_pred,
        geo_pred,
        gnn_pred,
        lower_is_better
      ),
      ~ {
        vals <- c(...)[1:5]
        models <- c(
          "cat_pred",
          "embed_pred",
          "graph_pred",
          "geo_pred",
          "gnn_pred"
        )
        lower <- ..6
        if (lower) {
          models[which.min(vals)]
        } else {
          models[which.max(vals)]
        }
      }
    )
  ) %>%
  mutate(across(
    cat_pred:gnn_pred,
    ~ ifelse(abs(.) < 1, sprintf("%.3f", .), sprintf("%.1f", .))
  ))

comparison_table

# ----------------------------------------------------------------------------

quantile_viz_data <- results %>%
  filter(grepl("quantile_loss", .metric)) %>%
  mutate(
    tau = case_when(
      .metric == "quantile_loss_0.5" ~ 0.50,
      .metric == "quantile_loss_0.75" ~ 0.75,
      .metric == "quantile_loss_0.9" ~ 0.90,
      .metric == "quantile_loss_0.95" ~ 0.95
    )
  )

num_models <- length(unique(quantile_viz_data$model))
model_colors <- setNames(
  viridis::magma(num_models, end = 0.85),
  unique(quantile_viz_data$model)
)

ggplot(
  quantile_viz_data,
  aes(x = tau, y = .estimate, color = model, group = model)
) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = model_colors, name = "") +
  labs(
    title = "Quantile Loss Across Percentiles",
    x = "Quantile",
    y = "Quantile Loss"
  ) +
  theme_bw(base_size = 22)

claim_viz_data <- claim_size_results %>%
  mutate(
    model = factor(model)
  )

all_models <- levels(claim_viz_data$model)
palette_magma <- viridis::magma(length(all_models), end = 0.9)

names(palette_magma) <- all_models

ggplot(
  claim_viz_data,
  aes(x = claim_size, y = smape, color = model, group = model)
) +
  geom_line(alpha = 0.8) +
  geom_point() +
  scale_color_manual(values = palette_magma, name = "") +
  labs(
    title = "Performance by Claim Size (SMAPE)",
    x = "Claim Size"
  ) +
  theme_bw(base_size = 22)

pred_df %>%
  mutate(exposure_band = cut_number(exposure, 4, labels = FALSE)) %>%
  pivot_longer(
    cols = c(cat_pred, geo_pred, graph_pred, embed_pred, gnn_pred)
  ) %>%
  group_by(name, exposure_band) %>%
  smape(loss_cost, value, case_weights = exposure) %>%
  ggplot(aes(x = exposure_band, y = .estimate, group = name, color = name)) +
  geom_line(alpha = 0.8) +
  geom_point() +
  scale_color_manual(values = palette_magma, name = "") +
  labs(
    title = "Performance by Exposure Percentile (SMAPE)",
    x = "Exposure Percentile",
    y = "smape"
  ) +
  theme_bw(base_size = 22)


# ============================================================================
# Spatial Dependence Analysis (Moran's I)
# ============================================================================
# Test for spatial autocorrelation in model residuals. Well-specified models
# should have minimal spatial dependence in residuals. High Moran's I indicates
# that the model is missing spatial patterns.
# ============================================================================
res_df <- train_data %>%
  mutate(
    cat_res = residuals(cat_model, type = "response"),
    geo_res = residuals(geo_model, type = "response"),
    graph_res = residuals(graph_model, type = "response"),
    embed_res = residuals(embed_model, type = "response"),
    gnn_res = loss_cost - lc_gnn
  )

res_df_agg <- res_df %>%
  left_join(pc_map %>% select(POSTCODE), by = join_by(pc == POSTCODE)) %>%
  st_as_sf()

nb <- poly2nb(res_df_agg, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

MC_cat_res <- moran.mc(
  res_df_agg$cat_res,
  lw,
  nsim = 999,
  alternative = "greater"
)
MC_geo_res <- moran.mc(
  res_df_agg$geo_res,
  lw,
  nsim = 999,
  alternative = "greater"
)
MC_graph_res <- moran.mc(
  res_df_agg$graph_res,
  lw,
  nsim = 999,
  alternative = "greater"
)
MC_embed_res <- moran.mc(
  res_df_agg$embed_res,
  lw,
  nsim = 999,
  alternative = "greater"
)

MC_gnn_res <- moran.mc(
  res_df_agg$gnn_res,
  lw,
  nsim = 999,
  alternative = "greater"
)


moran(res_df_agg$cat_res, lw, length(nb), Szero(lw))[1]
moran(res_df_agg$geo_res, lw, length(nb), Szero(lw))[1]
moran(res_df_agg$graph_res, lw, length(nb), Szero(lw))[1]
moran(res_df_agg$embed_res, lw, length(nb), Szero(lw))[1]
moran(res_df_agg$gnn_res, lw, length(nb), Szero(lw))[1]


MC_cat_res$p.value
MC_geo_res$p.value
MC_graph_res$p.value
MC_embed_res$p.value
MC_gnn_res$p.value
