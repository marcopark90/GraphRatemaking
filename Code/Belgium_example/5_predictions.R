# Predictions -------------------------------------------------------------

pred_df <- test_data %>%
  mutate(
    geo_pred = predict(geo_model, newdata = ., type = "response") / exposure,
    graph_pred = predict(graph_model, newdata = ., type = "response") /
      exposure,
    embed_pred = predict(embed_model, newdata = ., type = "response") /
      exposure,
    gnn_pred = predict(gnn_model, newdata = ., type = "response") / exposure
  )

pred_df %$% weighted.mean(loss_cost, exposure)
pred_df %$% weighted.mean(geo_pred, exposure)
pred_df %$% weighted.mean(graph_pred, exposure)
pred_df %$% weighted.mean(embed_pred, exposure)
pred_df %$% weighted.mean(gnn_pred, exposure)

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

lf_geo <- pred_df %>%
  lift_curve(geo_pred, loss_cost, exposure, relative = TRUE) +
  ggtitle("Geo Pred")

lf_graph <- pred_df %>%
  lift_curve(graph_pred, loss_cost, exposure, relative = TRUE) +
  ggtitle("Graph Pred")

lf_embed <- pred_df %>%
  lift_curve(embed_pred, loss_cost, exposure, relative = TRUE) +
  ggtitle("Embed Pred")

lg_gnn <- pred_df %>%
  lift_curve(gnn_pred, loss_cost, exposure, relative = TRUE) +
  ggtitle("GNN Pred")

lf_charts <- wrap_plots(list(lf_geo, lf_graph, lf_embed, lg_gnn))

ggsave(
  "./Graphs/lf_charts.png",
  lf_charts,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "px",
  dpi = 300
)

lf_top_geo <- pred_df %>%
  lift_curve(geo_pred, loss_cost, exposure, relative = TRUE, top_perc = 10) +
  ggtitle("Geo Pred (90th Percentile)")

lf_top_graph <- pred_df %>%
  lift_curve(graph_pred, loss_cost, exposure, relative = TRUE, top_perc = 10) +
  ggtitle("Graph Pred (90th Percentile)")

lf_top_embed <- pred_df %>%
  lift_curve(embed_pred, loss_cost, exposure, relative = TRUE, top_perc = 10) +
  ggtitle("Embed Pred (90th Percentile)")

lg_top_gnn <- pred_df %>%
  lift_curve(gnn_pred, loss_cost, exposure, relative = TRUE, top_perc = 10) +
  ggtitle("GNN Pred (90th Percentile)")

lf_top_charts <- wrap_plots(list(
  lf_top_geo,
  lf_top_graph,
  lf_top_embed,
  lg_top_gnn
))

ggsave(
  "./Graphs/lf_top_charts.png",
  lf_top_charts,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)

gini_geo <- pred_df %>%
  gini_index(amount, geo_pred, exposure, "Geo Pred")

gini_graph <- pred_df %>%
  gini_index(amount, graph_pred, exposure, "Graph Pred")

gini_embed <- pred_df %>%
  gini_index(amount, embed_pred, exposure, "Embed Pred")

gini_gnn <- pred_df %>%
  gini_index(amount, gnn_pred, exposure, "GNN Pred")

gini_charts <- wrap_plots(list(gini_geo, gini_graph, gini_embed, gini_gnn))

ggsave(
  "./Graphs/gini_charts.png",
  gini_charts,
  width = zoom_dims[1],
  height = zoom_dims[2],
  units = "in",
  dpi = 300
)
