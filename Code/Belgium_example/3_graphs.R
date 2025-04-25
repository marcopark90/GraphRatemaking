# Graphs ------------------------------------------------------------------

num_vars <- agg_one_way(agec, ageph, bm, power) %>%
  wrap_plots() +
  plot_annotation(title = "Numerical Variables")

ggsave(
  "./Graphs/continuous_vars.png",
  num_vars,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

cat_vars <- agg_one_way(coverage, fuel, use, fleet, sex, numerical = FALSE) %>%
  wrap_plots() +
  plot_annotation(title = "Categorical Variables")

ggsave(
  "./Graphs/binned_vars.png",
  cat_vars,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

lc_dens <- claims %>%
  mutate(loss_cost = amount / exposure) %>%
  ggplot() +
  geom_density(aes(x = loss_cost), color = "#51127c", linewidth = 2) +
  scale_x_continuous(transform = "log10") +
  theme_bw(base_size = 22) +
  ggtitle("Loss Cost Density") +
  xlab("Log(Los Cost)")

ggsave(
  "./Graphs/claim_density.png",
  lc_dens,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

lc_map <- ggplot() +
  geom_sf(data = pc_map, fill = "white") +
  geom_sf(data = claims_agg, aes(fill = loss_cost)) +
  scale_fill_viridis(option = "F", name = "", trans = "log10") +
  theme_bw() +
  ggtitle("Loss Cost") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/loss_cost_map.png",
  lc_map,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

gnn_map_graph <- ggplot() +
  geom_sf(data = gnn_map, aes(fill = lc_gnn)) +
  scale_fill_viridis(option = "magma", name = "", begin = 0) +
  theme_bw() +
  ggtitle("GNN LC Prediction") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/loss_cost_gnn_pred.png",
  gnn_map_graph,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

gnn_bins_map_graph <- gnn_map %>%
  mutate(
    lc_gnn_bins = cut(
      lc_gnn,
      breaks = classIntervals(lc_gnn, 5, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ) %>%
      as_factor()
  ) %>%
  ggplot() +
  geom_sf(aes(fill = lc_gnn_bins)) +
  scale_fill_viridis(option = "magma", name = "", discrete = TRUE) +
  theme_bw() +
  ggtitle("GNN LC Binned Prediction") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/loss_cost_gnn_bins_pred.png",
  gnn_bins_map_graph,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

gnn_map <- wrap_plots(list(gnn_map_graph, gnn_bins_map_graph))

ggsave(
  "./Graphs/gnn_map.png",
  gnn_map,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

hex_map_plot <- hex_map %>%
  ggplot() +
  geom_sf() +
  theme_bw() +
  ggtitle("Hex Grid") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/hex_map.png",
  hex_map_plot,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

zip_codes_map <- pc_map %>%
  ggplot() +
  geom_sf() +
  theme_bw() +
  ggtitle("Zip Codes") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/zip_codes.png",
  zip_codes_map,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

simple_graph <- make_graph(c(
  "Node",
  "1",
  "Node",
  "2",
  "Node",
  "3",
  "Node",
  "4",
  "Node",
  "5",
  "Node",
  "6",
  "Node",
  "7",
  "Node",
  "8"
))

simple_plot <- ggplot(data = ggnetwork(simple_graph, layout = as_star())) +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_nodes(aes(x = x, y = y), size = 20, color = "#7f7f7f", shape = 15) +
  geom_nodetext(aes(x = x, y = y, label = name), size = 10) +
  ggtitle("Euclidean Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none") +
  theme_void(base_size = 22)

ggsave(
  "./Graphs/graph_simple.png",
  simple_plot,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

bigger_graph <- make_graph(c("1", "2", "2", "3", "2", "4", "1", "5", "5", "6"))

bigger_plot <- ggplot(data = ggnetwork(bigger_graph, layout = as_tree())) +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_nodes(aes(x = x, y = y), size = 20, color = "#7f7f7f") +
  geom_nodetext(aes(x = x, y = y, label = name), size = 10) +
  ggtitle("Non-Euclidean Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none") +
  theme_void(base_size = 22)

ggsave(
  "./Graphs/graph_bigger.png",
  bigger_plot,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

comm_graph <- make_graph(c("a", "b", 
                           "b", "c", 
                           "b", "h", 
                           "c", "h", 
                           "c", "d",
                           "c", "e",
                           "d", "h",
                           "d", "e",
                           "c", "d",
                           "e", "f",
                           "f", "g",
                           "g", "h"))

E(comm_graph)$weight <- c(1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1)

comm_plot <- ggplot(data = ggnetwork(comm_graph)) +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend, size = weight)) +
  geom_nodes(aes(x = x, y = y), size = 20, color = "#7f7f7f") +
  geom_nodetext(aes(x = x, y = y, label = name), size = 10) +
  ggtitle("Densest Community") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none", size = "none") +
  theme_void(base_size = 22) +
  scale_size(range = c(0.5, 3))

ggsave(
  "./Graphs/graph_comm.png",
  comm_plot,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

graph_cluster <- ggplot(
  data = ggnetwork(
    graph_data,
    layout = st_centroid(pc_map) %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
  scale_fill_viridis(option = "magma", discrete = TRUE, guide = "none") +
  geom_edges(
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = .5,
    color = "#7f7f7f"
  ) +
  geom_nodes(aes(x = x, y = y), color = "#7f7f7f") +
  theme_bw() +
  ggtitle("Graph Clusters") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none")

ggsave(
  "./Graphs/graph_cluster.png",
  graph_cluster,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

graph_data_full <- reduce(
  1:n_cl,
  ~ add_edges(
    .x,
    combn(which(membership(cl) == .y), 2, simplify = FALSE) %>% list_c()
  ),
  .init = graph_data
)

full_graph <- ggplot(
  data = ggnetwork(
    graph_data_full,
    layout = st_centroid(pc_map) %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
  scale_fill_viridis(option = "magma", discrete = TRUE, guide = "none") +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend), linewidth = .5) +
  geom_nodes(aes(x = x, y = y), color = "black", size = 1) +
  theme_bw() +
  ggtitle("Graph Clusters") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none")

ggsave(
  "./Graphs/full_graph.png",
  full_graph,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

ind_plot <- map(
  1:n_cl,
  ~ ggplot(
    data = ggnetwork(
      subgraph(graph_data, which(membership(cl) == .x)),
      layout = st_centroid(pc_map) %>%
        filter(cluster == .x) %>%
        st_coordinates(),
      scale = FALSE
    )
  ) +
    geom_sf(
      data = pc_map %>% filter(cluster == .x),
      fill = viridis(n_cl, option = "magma")[.x]
    ) +
    geom_edges(
      aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = .25,
      color = "#7f7f7f"
    ) +
    geom_nodes(aes(x = x, y = y), size = .5, color = "#7f7f7f") +
    theme_bw() +
    ggtitle(glue::glue("Graph Area {.x}")) +
    theme(
      plot.title = element_text(size = 22),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    guides(fill = "none", color = "none") +
    coord_sf(xlim = st_bbox(pc_map)[c(1, 3)], ylim = st_bbox(pc_map)[c(2, 4)])
) %>%
  wrap_plots(nrow = 2, ncol = 5)

ggsave(
  "./Graphs/ind.png",
  ind_plot,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

map_graph <- ggplot(
  data = ggnetwork(
    subgraph(graph_data, all_idx),
    layout = st_centroid(pc_map) %>% extract(all_idx, ) %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
  geom_edges(
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = .5,
    color = "#7f7f7f"
  ) +
  geom_nodes(aes(x = x, y = y), size = 1, color = "#7f7f7f") +
  scale_fill_viridis(option = "magma", discrete = TRUE, guide = "none") +
  theme_bw() +
  ggtitle("Full Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none")

map_train <- ggplot(
  data = ggnetwork(
    subgraph(graph_data, train_idx),
    layout = st_centroid(pc_map) %>% extract(train_idx, ) %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
  geom_edges(
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = .5,
    color = "#7f7f7f"
  ) +
  geom_nodes(aes(x = x, y = y), size = 1, color = "#7f7f7f") +
  scale_fill_viridis(option = "magma", discrete = TRUE, guide = "none") +
  theme_bw() +
  ggtitle("Train Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none")

map_val <- ggplot(
  data = ggnetwork(
    subgraph(graph_data, val_idx),
    layout = st_centroid(pc_map) %>% extract(val_idx, ) %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
  geom_edges(
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = .5,
    color = "#7f7f7f"
  ) +
  geom_nodes(aes(x = x, y = y), size = 1, color = "#7f7f7f") +
  scale_fill_viridis(option = "magma", discrete = TRUE, guide = "none") +
  theme_bw() +
  ggtitle("Validation Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none")

map_test <- ggplot(
  data = ggnetwork(
    subgraph(graph_data, test_idx),
    layout = st_centroid(pc_map) %>% extract(test_idx, ) %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
  geom_edges(
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = .5,
    color = "#7f7f7f"
  ) +
  geom_nodes(aes(x = x, y = y), size = 1, color = "#7f7f7f") +
  scale_fill_viridis(option = "magma", discrete = TRUE, guide = "none") +
  theme_bw() +
  ggtitle("Test Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = "none", color = "none")

graphs_split <- patchwork::wrap_plots(
  list(map_graph, map_train, map_val, map_test),
  nrow = 2,
  ncol = 2
)

ggsave(
  "./Graphs/graphs_split.png",
  graphs_split,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

tsne_clusters <- tsne_embeddings %>%
  as_tibble() %>%
  mutate(cluster = k_clusters %>% use_series(cluster) %>% as_factor()) %>%
  ggplot() +
  geom_point(
    aes(x = V1, y = V2, fill = cluster),
    pch = 21,
    size = 2,
    color = "black"
  ) +
  geom_circle(
    data = cent_dist,
    aes(x0 = center_x, y0 = center_y, r = distance),
    color = "red",
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_fill_viridis(option = "magma", discrete = TRUE, guide = NULL) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 22)
  ) +
  ggtitle("t-SNE Clusters")

ggsave(
  "./Graphs/tsne_clusters.png",
  tsne_clusters,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)
