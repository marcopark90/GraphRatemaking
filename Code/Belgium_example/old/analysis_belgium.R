library(mgcv)
library(glue)
library(tidyverse)
library(tidymodels)
library(magrittr)
library(sf)
library(gstat)
library(tweedie)
library(statmod)
library(pracma)
library(fields)
library(units)
library(igraph)
library(ggnetwork)
library(scales)
library(viridis)
library(classInt)

sf_use_s2(FALSE)

source("./Code/utils.R")

set.seed(999)

zoom_dims <- dev.size("in")

# Start Analysis ----------------------------------------------------------

claims <- read_csv("./Data/Belgium/claims_belgium.csv")
pc_map <- st_read("./Data/Belgium/Belgium.shp") %>% st_make_valid()
gnn_map <- st_read("./Data/Belgium//Belgium_GNN.shp") %>% st_make_valid()

claims_agg <- claims %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs("WGS84")) %>%
  st_join(pc_map, ., join = st_contains, left = TRUE) %>%
  drop_na() %>%
  group_by(geometry) %>%
  summarise(amount = sum(amount), exposure = sum(exposure)) %>%
  mutate(loss_cost = amount / exposure)

ggplot() +
  geom_sf(data = pc_map, fill = "white") +
  geom_sf(data = claims_agg, aes(fill = log10(loss_cost))) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Loss Cost") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/loss_cost_map.png",
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

ggplot() +
  geom_sf(data = gnn_map, aes(fill = lc_gnn)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("GNN LC Prediction") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/loss_cost_gnn_pred.png",
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

pc_map <- pc_map %>%
  st_join(gnn_map, join = st_contains) %>%
  st_join(claims_agg, join = st_contains) %>%
  select(-starts_with("X")) %>%
  mutate(
    area = st_area(pc_map) %>% set_units(km^2) %>% as.numeric(),
    per = st_length(pc_map) %>% set_units(km) %>% as.numeric()
  )

all_idx <- which(!is.na(pc_map$exposure))

splits <- initial_validation_split(enframe(all_idx, name = NULL))
train_idx <- training(splits) %>% pull(value)
val_idx <- validation(splits) %>% pull(value)
test_idx <- testing(splits) %>% pull(value)

# hex <- st_make_grid(pc_map, cellsize = .1, square = FALSE)
#
# hex[pc_map] %>%
#   ggplot() +
#   geom_sf()  +
#   theme_bw() +
#   ggtitle("Hex Grid") +
#   theme(plot.title = element_text(size = 22))

pc_map %>%
  ggplot() +
  geom_sf() +
  theme_bw() +
  ggtitle("Zip Codes") +
  theme(plot.title = element_text(size = 22))

ggsave(
  "./Graphs/zip_codes.png",
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

graph_data <- graph_from_adj_list(st_touches(pc_map), mode = "all")

cl <- cluster_fast_greedy(graph_data)

n_cl <- max(membership(cl))

pc_map <- pc_map %>%
  mutate(
    deg_cent = centr_degree(graph_data)$res,
    eig_cent = centr_eigen(graph_data)$vector,
    clos_cent = centr_clo(graph_data)$res,
    bet_cent = centr_betw(graph_data)$res,
    cluster = as.factor(membership(cl))
  )

centr <- st_centroid(pc_map)

ggplot(
  data = ggnetwork(
    graph_data,
    layout = centr %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
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
  "./Graphs/graph_cluster.png",
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

ggplot(
  data = ggnetwork(
    graph_data_full,
    layout = centr %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map, aes(fill = cluster)) +
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

ind_plots <- map(
  1:n_cl,
  ~ ggplot(
    data = ggnetwork(
      subgraph(graph_data, which(membership(cl) == .x)),
      layout = centr %>% filter(cluster == .x) %>% st_coordinates(),
      scale = FALSE
    )
  ) +
    geom_sf(
      data = pc_map %>% filter(cluster == .x),
      fill = hue_pal()(n_cl)[.x]
    ) +
    geom_edges(aes(x = x, y = y, xend = xend, yend = yend), linewidth = .5) +
    geom_nodes(aes(x = x, y = y), size = 1) +
    theme_bw() +
    ggtitle(glue::glue("Graph Area {.x}")) +
    theme(
      plot.title = element_text(size = 22),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    guides(fill = "none", color = "none") +
    coord_sf(xlim = st_bbox(pc_map)[c(1, 3)], ylim = st_bbox(pc_map)[c(2, 4)])
)

ind <- patchwork::wrap_plots(ind_plots, nrow = 2, ncol = 5)

ggsave(
  "./Graphs/ind.png",
  ind,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

map_graph <- ggplot(
  data = ggnetwork(
    graph_data,
    layout = centr %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map) +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend), linewidth = .5) +
  geom_nodes(aes(x = x, y = y), color = "black", size = 1) +
  theme_bw() +
  ggtitle("Full Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

map_train <- ggplot(
  data = ggnetwork(
    subgraph(graph_data, train_idx),
    layout = centr[train_idx, ] %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map) +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend), linewidth = .5) +
  geom_nodes(aes(x = x, y = y), color = "black", size = 1) +
  theme_bw() +
  ggtitle("Train Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

map_val <- ggplot(
  data = ggnetwork(
    subgraph(graph_data, val_idx),
    layout = centr[val_idx, ] %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map) +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend), linewidth = .5) +
  geom_nodes(aes(x = x, y = y), color = "black", size = 1) +
  theme_bw() +
  ggtitle("Validation Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

map_test <- ggplot(
  data = ggnetwork(
    subgraph(graph_data, test_idx),
    layout = centr[test_idx, ] %>% st_coordinates(),
    scale = FALSE
  )
) +
  geom_sf(data = pc_map) +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend), linewidth = .5) +
  geom_nodes(aes(x = x, y = y), color = "black", size = 1) +
  theme_bw() +
  ggtitle("Test Graph") +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

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

# Train / Test Split ------------------------------------------------------

modeling_data <- claims %>%
  mutate(
    loss_cost = amount / exposure,
    loss_cost = if_else(
      loss_cost > quantile(loss_cost, .99),
      quantile(loss_cost, .99),
      loss_cost
    ),
    amount = loss_cost * exposure
  ) %>%
  left_join(
    pc_map %>%
      st_drop_geometry() %>%
      select(
        POSTCODE,
        lc_gnn,
        area,
        per,
        deg_cent,
        eig_cent,
        clos_cent,
        bet_cent,
        cluster,
        starts_with("V")
      ),
    by = join_by(pc == POSTCODE)
  )

data_split <- initial_split(modeling_data, prop = .80, )

train_data <- training(data_split)

test_data <- testing(data_split)

# Modeling ----------------------------------------------------------------

geo_model <- gam(
  amount ~
    coverage +
      fuel +
      use +
      fleet +
      sex +
      ageph +
      bm +
      agec +
      power +
      s(long, lat, bs = "tp") +
      offset(log(exposure)),
  family = tw(link = "log"),
  data = train_data,
  method = "REML"
)

graph_model <- gam(
  amount ~
    coverage +
      fuel +
      use +
      fleet +
      sex +
      ageph +
      bm +
      agec +
      power +
      deg_cent +
      eig_cent +
      clos_cent +
      bet_cent +
      cluster +
      s(long, lat, bs = "tp") +
      offset(log(exposure)),
  family = tw(link = "log"),
  data = train_data,
  method = "REML"
)

embed_model <- gam(
  amount ~
    coverage +
      fuel +
      use +
      fleet +
      sex +
      ageph +
      bm +
      agec +
      power +
      V0 +
      V1 +
      V2 +
      V3 +
      V4 +
      V5 +
      V6 +
      V7 +
      s(long, lat, bs = "tp") +
      offset(log(exposure)),
  family = tw(link = "log"),
  data = train_data,
  method = "REML"
)

gnn_model <- gam(
  amount ~
    coverage +
      fuel +
      use +
      fleet +
      sex +
      ageph +
      bm +
      agec +
      power +
      lc_gnn +
      s(long, lat, bs = "tp") +
      offset(log(exposure)),
  family = tw(link = "log"),
  data = train_data,
  method = "REML"
)

# Predict -----------------------------------------------------------------

pred_df <- test_data %>%
  mutate(
    geo_pred = predict(geo_model, newdata = ., type = "response") / exposure,
    graph_pred = predict(graph_model, newdata = ., type = "response") /
      exposure,
    embed_pred = predict(embed_model, newdata = ., type = "response") /
      exposure,
    gnn_pred = predict(gnn_model, newdata = ., type = "response") / exposure
  )

pred_df %$% mean(loss_cost)
pred_df %$% mean(geo_pred)
pred_df %$% mean(graph_pred)
pred_df %$% mean(embed_pred)
pred_df %$% mean(gnn_pred)

# pred_df %>%
#   mutate(loss_cost = if_else(loss_cost == 0, abs(jitter(loss_cost)), loss_cost),
#          geo_pred = if_else(geo_pred == 0, abs(jitter(geo_pred)), geo_pred)) %>%
#   select(geo_pred, loss_cost, exposure) %>%
#   arrange(geo_pred) %>%
#   mutate(buckets = cut_interval(cumsum(exposure), n = 10, labels = 1:10)) %>%
#   select(-exposure) %>%
#   pivot_longer(-buckets) %>%
#   ggplot() +
#   geom_boxplot(aes(x = buckets, y = value, color = name)) +
#   scale_y_log10()

lf_geo <- pred_df %>%
  lift_curve_table(geo_pred, loss_cost, exposure, 10) %>%
  lift_curve_plot() +
  theme_bw() +
  ggtitle("Lift Curve Geographical") +
  theme(plot.title = element_text(size = 22), legend.position = "bottom")

lf_graph <- pred_df %>%
  lift_curve_table(graph_pred, loss_cost, exposure, 10) %>%
  lift_curve_plot() +
  ggtitle("Lift Curve Graph") +
  theme_bw() +
  theme(plot.title = element_text(size = 22), legend.position = "bottom")

lf_embed <- pred_df %>%
  lift_curve_table(embed_pred, loss_cost, exposure, 10) %>%
  lift_curve_plot() +
  ggtitle("Lift Curve Node Embeddings") +
  theme_bw() +
  theme(plot.title = element_text(size = 22), legend.position = "bottom")

lf_gnn <- pred_df %>%
  lift_curve_table(gnn_pred, loss_cost, exposure, 10) %>%
  lift_curve_plot() +
  ggtitle("Lift Curve GNN") +
  theme_bw() +
  theme(plot.title = element_text(size = 22), legend.position = "bottom")

lifts <- patchwork::wrap_plots(
  list(lf_geo, lf_graph, lf_embed, lf_gnn),
  nrow = 2,
  ncol = 2
)

ggsave(
  "./Graphs/lift_curves.png",
  lifts,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

pred_df %>%
  rmse(loss_cost, geo_pred, case_weights = exposure) %>%
  pull(.estimate)
pred_df %>%
  rmse(loss_cost, graph_pred, case_weights = exposure) %>%
  pull(.estimate)
pred_df %>%
  rmse(loss_cost, embed_pred, case_weights = exposure) %>%
  pull(.estimate)
pred_df %>%
  rmse(loss_cost, gnn_pred, case_weights = exposure) %>%
  pull(.estimate)

pred_df %>% gini_value(loss_cost, geo_pred, exposure)
pred_df %>% gini_value(loss_cost, graph_pred, exposure)
pred_df %>% gini_value(loss_cost, embed_pred, exposure)
pred_df %>% gini_value(loss_cost, gnn_pred, exposure)

gp_geo <- pred_df %>%
  gini_plot(loss_cost, geo_pred, exposure) +
  ggtitle(glue(
    "Gini Plot Geographical Model 
               Gini Value = {round(pred_df %>% gini_value(loss_cost, geo_pred, exposure),4)}"
  )) +
  theme_bw() +
  theme(plot.title = element_text(size = 22))

gp_graph <- pred_df %>%
  gini_plot(loss_cost, graph_pred, exposure) +
  ggtitle(glue(
    "Gini Plot Graph Model 
               Gini Value = {round(pred_df %>% gini_value(loss_cost, graph_pred, exposure),4)}"
  )) +
  theme_bw() +
  theme(plot.title = element_text(size = 22))

gp_embed <- pred_df %>%
  gini_plot(loss_cost, embed_pred, exposure) +
  ggtitle(glue(
    "Gini Plot Embeddings Model 
               Gini Value = {round(pred_df %>% gini_value(loss_cost, embed_pred, exposure),4)}"
  )) +
  theme_bw() +
  theme(plot.title = element_text(size = 22))

gp_gnn <- pred_df %>%
  gini_plot(loss_cost, gnn_pred, exposure) +
  ggtitle(glue(
    "Gini Plot GNN Model 
               Gini Value = {round(pred_df %>% gini_value(loss_cost, gnn_pred, exposure),4)}"
  )) +
  theme_bw() +
  theme(plot.title = element_text(size = 22))

gini <- patchwork::wrap_plots(
  list(gp_geo, gp_graph, gp_embed, gp_gnn),
  nrow = 2,
  ncol = 2
)

ggsave(
  "./Graphs/gini.png",
  gini,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)


# pc_comp <- pred_df %>%
#   arrange(pc) %>%
#   mutate(pc = cut_number(pc, n = 25, labels = 1:25)) %>%
#   group_by(pc) %>%
#   summarise(expo = sum(exposure),
#             loss_cost = weighted.mean(loss_cost, exposure),
#             geo_pred = weighted.mean(geo_pred, exposure),
#             graph_pred = weighted.mean(graph_pred, exposure),
#             embed_pred = weighted.mean(embed_pred, exposure),
#             gnn_pred = weighted.mean(gnn_pred, exposure)) %>%
#   pivot_longer(cols = -c(pc, expo))
#
# pc_comp %>%
#   ggplot() +
#   geom_bar(aes(x = pc, y = 1/5*expo*(min(value)/min(expo))), stat = "identity", fill = "gray") +
#   geom_line(aes(x = pc, y = value, group = name, color = name)) +
#   scale_y_continuous(name = "Loss Cost",
#     sec.axis = sec_axis(~./(min(pc_comp$value)/min(pc_comp$expo)), name = "Exposure")) +
#   ggtitle("PC AvsE") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22), legend.position = "bottom")

# pred_df_pc <- pred_df %>%
#   group_by(pc) %>%
#   summarise(expo = sum(exposure),
#             loss_cost = weighted.mean(loss_cost, exposure),
#             geo_pred = weighted.mean(geo_pred, exposure),
#             graph_pred = weighted.mean(graph_pred, exposure),
#             embed_pred = weighted.mean(embed_pred, exposure),
#             gnn_pred = weighted.mean(gnn_pred, exposure))
#
# pc_geo <-  pred_df_pc %>%
#   lift_curve_table(geo_pred, loss_cost, expo, 25) %>%
#   lift_curve_plot() +
#   ggtitle("PC Lift Curve Geographical") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22), legend.position = "bottom")
#
# pc_graph <- pred_df_pc %>%
#   lift_curve_table(graph_pred, loss_cost, expo, 25) %>%
#   lift_curve_plot() +
#   ggtitle("PC Lift Curve Graph") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22), legend.position = "bottom")
#
# pc_embed <- pred_df_pc %>%
#   lift_curve_table(embed_pred, loss_cost, expo, 25) %>%
#   lift_curve_plot() +
#   ggtitle("PC Lift Curve Node Embeddings") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22), legend.position = "bottom")
#
# pc_gnn <- pred_df_pc %>%
#   lift_curve_table(gnn_pred, loss_cost, expo, 25) %>%
#   lift_curve_plot() +
#   ggtitle("PC Lift Curve GNN") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22), legend.position = "bottom")
#
# patchwork::wrap_plots(list(pc_geo, pc_graph, pc_embed, pc_gnn),
#                       nrow = 2, ncol = 2)
#
#
# gp_geo <- pred_df_pc %>% gini_plot(loss_cost, geo_pred, expo) +
#   ggtitle(glue("PC Gini Plot Geographical Model
#                Gini Value = {round(pred_df_pc %>% gini_value(loss_cost, geo_pred, expo),4)}")) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22))
#
# gp_graph <- pred_df_pc %>% gini_plot(loss_cost, graph_pred, expo) +
#   ggtitle(glue("PC Gini Plot Graph Model
#                Gini Value = {round(pred_df_pc %>% gini_value(loss_cost, graph_pred, expo),4)}")) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22))
#
# gp_embed <- pred_df_pc %>% gini_plot(loss_cost, embed_pred, expo) +
#   ggtitle(glue("PC Gini Plot Embeddings Model
#                Gini Value = {round(pred_df_pc %>% gini_value(loss_cost, embed_pred, expo),4)}")) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22))
#
# gp_gnn <- pred_df_pc %>% gini_plot(loss_cost, gnn_pred, expo) +
#   ggtitle(glue("PC Gini Plot GNN Model
#                Gini Value = {round(pred_df_pc %>% gini_value(loss_cost, gnn_pred, expo),4)}")) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 22))
#
# patchwork::wrap_plots(list(gp_geo, gp_graph, gp_embed, gp_gnn),
#                       nrow = 2, ncol = 2)

# Plot Smoothing ----------------------------------------------------------

centr_score <- centr %>%
  mutate(
    st_coordinates(centr) %>% as_tibble() %>% rename(long = X, lat = Y),
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
      breaks = classIntervals(geo_smooth, 10, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ),
    graph_class = cut(
      graph_smooth,
      breaks = classIntervals(graph_smooth, 10, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ),
    embed_class = cut(
      embed_smooth,
      breaks = classIntervals(embed_smooth, 10, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    ),
    gnn_class = cut(
      gnn_smooth,
      breaks = classIntervals(gnn_smooth, 10, style = "kmeans")$brks,
      right = FALSE,
      include.lowest = TRUE,
      labels = FALSE
    )
  )

geo_score <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = geo_smooth)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Geographical Score") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

graph_score <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = graph_smooth)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Graph Score") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

embed_score <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = embed_smooth)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Node Embed. Score") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

gnn_score <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = gnn_smooth)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("GNN Score") +
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
)

ggsave(
  "./Graphs/geo_scores.png",
  geo_scores,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

geo_class <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = geo_class)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Geographical Class") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

graph_class <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = graph_class)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Graph Class") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

embed_class <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = embed_class)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Node Embeddings Class") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

gnn_class <- pc_map %>%
  st_join(centr_score) %>%
  ggplot() +
  geom_sf(aes(fill = gnn_class)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("GNN Class") +
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
)

ggsave(
  "./Graphs/geo_classes.png",
  geo_classes,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

# Territorial Categories --------------------------------------------------

# train_data <- train_data %>%
#   left_join(centr_score %>%
#               st_drop_geometry() %>%
#               select(POSTCODE, geo_class, graph_class, embed_class, gnn_class),
#               join_by(pc == POSTCODE)) %>%
#   mutate(geo_class = as.factor(geo_class),
#          graph_class = as.factor(graph_class),
#          embed_class = as.factor(embed_class),
#          gnn_class = as.factor(gnn_class))
#
# geo_class_terms <- gam(amount ~ coverage + fuel + use + fleet + sex + ageph +
#                    bm + agec + power + geo_class + offset(log(exposure)),
#                  family = tw(link = "log"),
#                  data = train_data) %>%
#   tidy(parametric = TRUE) %>%
#   select(term, estimate) %>%
#   filter(str_detect(term, "^geo_class")) %>%
#   bind_rows(tibble(term = "geo_class1", estimate = 0)) %>%
#   mutate(geo_estimate = exp(estimate),
#          term = str_remove(term, "geo_class") %>% as.integer(),
#          geo_rank = rank(estimate)) %>%
#   select(term, geo_estimate, geo_rank)
#
# graph_class_terms <- gam(amount ~ coverage + fuel + use + fleet + sex + ageph +
#                          bm + agec + power + graph_class + offset(log(exposure)),
#                        family = tw(link = "log"),
#                        data = train_data) %>%
#   tidy(parametric = TRUE) %>%
#   select(term, estimate) %>%
#   filter(str_detect(term, "^graph_class")) %>%
#   bind_rows(tibble(term = "graph_class1", estimate = 0)) %>%
#   mutate(graph_estimate = exp(estimate),
#          term = str_remove(term, "graph_class") %>% as.integer(),
#          graph_rank = rank(estimate)) %>%
#   select(term, graph_estimate, graph_rank)
#
# embed_class_terms <- gam(amount ~ coverage + fuel + use + fleet + sex + ageph +
#                            bm + agec + power + embed_class + offset(log(exposure)),
#                          family = tw(link = "log"),
#                          data = train_data) %>%
#   tidy(parametric = TRUE) %>%
#   select(term, estimate) %>%
#   filter(str_detect(term, "^embed_class")) %>%
#   bind_rows(tibble(term = "embed_class1", estimate = 0)) %>%
#   mutate(embed_estimate = exp(estimate),
#         term = str_remove(term, "embed_class") %>% as.integer(),
#          embed_rank = rank(estimate)) %>%
#   select(term, embed_estimate, embed_rank)
#
# gnn_class_terms <- gam(amount ~ coverage + fuel + use + fleet + sex + ageph +
#                            bm + agec + power + gnn_class + offset(log(exposure)),
#                          family = tw(link = "log"),
#                          data = train_data) %>%
#   tidy(parametric = TRUE) %>%
#   select(term, estimate) %>%
#   filter(str_detect(term, "^gnn_class")) %>%
#   bind_rows(tibble(term = "gnn_class1", estimate = 0)) %>%
#   mutate(gnn_estimate = exp(estimate),
#          term = str_remove(term, "gnn_class") %>% as.integer(),
#          gnn_rank = rank(estimate)) %>%
#   select(term, gnn_estimate, gnn_rank)
#
# centr_score <- centr_score %>%
#   left_join(geo_class_terms, join_by(geo_class == term)) %>%
#   left_join(graph_class_terms, join_by(graph_class == term)) %>%
#   left_join(embed_class_terms, join_by(embed_class == term)) %>%
#   left_join(gnn_class_terms, join_by(gnn_class == term))
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = geo_rank)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("Geographical Rank") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = graph_rank)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("Graph Rank") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = embed_rank)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("Node Embeddings Rank") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = gnn_rank)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("GNN Rank") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = geo_estimate)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("Geographical Estimate") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = graph_estimate)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("Graph Estimate") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = embed_estimate)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("Node Embeddings Estimate") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
#
# pc_map %>%
#   st_join(centr_score) %>%
#   ggplot() +
#   geom_sf(aes(fill = gnn_estimate)) +
#   scale_fill_viridis(option = "F") +
#   theme_bw() +
#   ggtitle("GNN Estimate") +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
