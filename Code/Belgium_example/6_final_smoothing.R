# Plot Smoothing ----------------------------------------------------------

centr_score <- pc_map %>%
  mutate(
    st_coordinates(st_centroid(.)) %>%
      as_tibble() %>%
      rename(long = X, lat = Y),
    exposure = 1,
    coverage = train_data$coverage[1],
    fuel = train_data$fuel[1],
    use = train_data$use[1],
    fleet = train_data$fleet[1],
    sex = train_data$sex[1],
    ageph = train_data$ageph[1],
    bm = train_data$bm[1],
    agec = train_data$agec[1],
    power = train_data$power[1]
  ) %>%
  mutate(
    geo_smooth = predict(
      geo_model,
      newdata = .,
      type = "terms",
      terms = "s(long,lat)"
    ) %>%
      as.vector(),
    graph_smooth = predict(
      graph_model,
      newdata = .,
      type = "terms",
      terms = "s(long,lat)"
    ) %>%
      as.vector(),
    embed_smooth = predict(
      embed_model,
      newdata = .,
      type = "terms",
      terms = "s(long,lat)"
    ) %>%
      as.vector(),
    gnn_smooth = predict(
      gnn_model,
      newdata = .,
      type = "terms",
      terms = "s(long,lat)"
    ) %>%
      as.vector(),
    geo_class = cut(
      geo_smooth,
      breaks = classIntervals(geo_smooth, 5, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ) %>%
      as_factor(),
    graph_class = cut(
      graph_smooth,
      breaks = classIntervals(graph_smooth, 5, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ) %>%
      as_factor(),
    embed_class = cut(
      embed_smooth,
      breaks = classIntervals(embed_smooth, 5, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ) %>%
      as_factor(),
    gnn_class = cut(
      gnn_smooth,
      breaks = classIntervals(gnn_smooth, 5, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ) %>%
      as_factor()
  )

geo_score <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = geo_smooth)) +
  scale_fill_viridis(option = "F", name = "") +
  theme_bw() +
  ggtitle("Geo Smoothed Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

graph_score <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = graph_smooth)) +
  scale_fill_viridis(option = "F", name = "") +
  theme_bw() +
  ggtitle("Graph Smoothed Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

embed_score <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = embed_smooth)) +
  scale_fill_viridis(option = "F", name = "") +
  theme_bw() +
  ggtitle("Embed Smoothed Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

gnn_score <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = gnn_smooth)) +
  scale_fill_viridis(option = "F", name = "") +
  theme_bw() +
  ggtitle("GNN Smoothed Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

geo_scores <- patchwork::wrap_plots(
  list(geo_score, graph_score, embed_score, gnn_score),
  nrow = 2,
  ncol = 2
) &
  theme(legend.position = 'bottom')

ggsave(
  "./Graphs/geo_scores.png",
  geo_scores,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

geo_class <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = geo_class)) +
  scale_fill_viridis(option = "F", discrete = TRUE, name = "") +
  theme_bw() +
  ggtitle("Geo Binned Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

graph_class <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = graph_class)) +
  scale_fill_viridis(option = "F", discrete = TRUE, name = "") +
  theme_bw() +
  ggtitle("Graph Binned Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

embed_class <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = embed_class)) +
  scale_fill_viridis(option = "F", discrete = TRUE, name = "") +
  theme_bw() +
  ggtitle("Embed Binned Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

gnn_class <- centr_score %>%
  ggplot() +
  geom_sf(aes(fill = gnn_class)) +
  scale_fill_viridis(option = "F", discrete = TRUE, name = "") +
  theme_bw() +
  ggtitle("GNN Binned Spatial Effect") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

geo_classes <- patchwork::wrap_plots(
  list(geo_class, graph_class, embed_class, gnn_class),
  nrow = 2,
  ncol = 2
) &
  theme(legend.position = 'bottom')

ggsave(
  "./Graphs/geo_classes.png",
  geo_classes,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)
