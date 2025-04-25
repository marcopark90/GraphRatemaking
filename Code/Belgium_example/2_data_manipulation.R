# Import Data & Manipulation ----------------------------------------------

data(beMTPL97)

claims <- beMTPL97 %>%
  as_tibble() %>%
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
    long,
    lat
  )

write_csv(claims, "./Data/Belgium/claims_belgium.csv")
pc_map <- st_read("./Data/Belgium/Belgium.shp") %>% st_make_valid()
gnn_map <- st_read("./Data/Belgium//Belgium_GNN.shp") %>% st_make_valid()

claims_agg <- claims %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs("WGS84")) %>%
  st_join(pc_map, ., join = st_contains, left = TRUE) %>%
  drop_na() %>%
  group_by(geometry) %>%
  summarise(amount = sum(amount), exposure = sum(exposure)) %>%
  mutate(loss_cost = amount / exposure)

pc_map <- pc_map %>%
  st_join(gnn_map, join = st_contains) %>%
  st_join(claims_agg, join = st_contains) %>%
  mutate(
    area = st_area(pc_map) %>% set_units(km^2) %>% as.numeric(),
    per = st_length(pc_map) %>% set_units(km) %>% as.numeric()
  )

all_idx <- which(!is.na(pc_map$exposure))

splits <- initial_validation_split(enframe(all_idx, name = NULL))
train_idx <- training(splits) %>% pull(value)
val_idx <- validation(splits) %>% pull(value)
test_idx <- testing(splits) %>% pull(value)

hex_map <- st_make_grid(pc_map, cellsize = .05, square = FALSE) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_join(pc_map, join = st_intersects, left = FALSE)

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

data_split <- initial_split(modeling_data, prop = .80, strata = amount)

train_data <- training(data_split)

test_data <- testing(data_split)

embeddings <- pc_map %>%
  st_drop_geometry() %>%
  select(cluster, starts_with("X"))

tsne_embeddings <- Rtsne(
  embeddings %>% select(cluster, starts_with("X")) %>% unique(),
  dims = 2
) %>%
  use_series(Y)

k_clusters <- kmeans(tsne_embeddings, 10)

centers <- k_clusters %>%
  use_series(centers) %>%
  as_tibble(.name_repair = ~ c("center_x", "center_y")) %>%
  mutate(cluster = factor(1:10))

cent_dist <- tsne_embeddings %>%
  as_tibble(.name_repair = ~ c("V1", "V2")) %>%
  mutate(cluster = k_clusters %>% use_series(cluster) %>% as_factor()) %>%
  left_join(centers) %>%
  mutate(distance = sqrt((V1 - center_x)^2 + (V2 - center_y)^2)) %>%
  group_by(cluster) %>%
  slice_max(distance, with_ties = FALSE) %>%
  select(center_x, center_y, distance)
