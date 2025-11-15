# ============================================================================
# Prediction Maps
# ============================================================================
# Generate spatial maps showing predictions from all models.
# Allows visual comparison of how different models predict loss cost
# across the Belgian territory.
# ============================================================================

# Generate predictions for all postal codes (including those without exposure)
# for mapping purposes
gnn_map_preds <- gnn_map %>%
  select(-exposure) %>%
  mutate(
    cat_pred = predict(cat_model, newdata = ., type = "response") %>%
      as.numeric(),
    geo_pred = predict(geo_model, newdata = ., type = "response") %>%
      as.numeric(),
    graph_pred = predict(graph_model, newdata = ., type = "response") %>%
      as.numeric(),
    embed_pred = predict(embed_model, newdata = ., type = "response") %>%
      as.numeric()
  )

cat_map_plot <- ggplot() +
  geom_sf(data = gnn_map_preds, aes(geometry = geometry, fill = cat_pred)) +
  scale_fill_viridis(
    option = "F",
    name = "",
    trans = "log10",
    na.value = "white"
  ) +
  theme_bw() +
  ggtitle("Province LC Prediction") +
  theme(plot.title = element_text(size = 22))


geo_map_plot <- ggplot() +
  geom_sf(data = gnn_map_preds, aes(geometry = geometry, fill = geo_pred)) +
  scale_fill_viridis(
    option = "F",
    name = "",
    trans = "log10",
    na.value = "white"
  ) +
  theme_bw() +
  ggtitle("Geographic LC Prediction") +
  theme(plot.title = element_text(size = 22))

graph_map_plot <- ggplot() +
  geom_sf(data = gnn_map_preds, aes(geometry = geometry, fill = graph_pred)) +
  scale_fill_viridis(
    option = "F",
    name = "",
    trans = "log10",
    na.value = "white"
  ) +
  theme_bw() +
  ggtitle("Graph LC Prediction") +
  theme(plot.title = element_text(size = 22))

embed_map_plot <- ggplot() +
  geom_sf(data = gnn_map_preds, aes(geometry = geometry, fill = embed_pred)) +
  scale_fill_viridis(
    option = "F",
    name = "",
    trans = "log10",
    na.value = "white"
  ) +
  theme_bw() +
  ggtitle("Embeddings LC Prediction") +
  theme(plot.title = element_text(size = 22))

gnn_map_plot <- ggplot() +
  geom_sf(data = gnn_map_preds, aes(geometry = geometry, fill = lc_gnn)) +
  scale_fill_viridis(
    option = "F",
    name = "",
    trans = "log10",
    na.value = "white"
  ) +
  theme_bw() +
  ggtitle("GNN LC Prediction") +
  theme(plot.title = element_text(size = 22))

wrap_plots(
  list(
    cat_map_plot,
    geo_map_plot,
    graph_map_plot,
    embed_map_plot,
    gnn_map_plot
  ),
  nrow = 2
)
