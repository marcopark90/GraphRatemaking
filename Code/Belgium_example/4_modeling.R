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

summary(geo_model)
summary(graph_model)
summary(embed_model)
summary(gnn_model)
