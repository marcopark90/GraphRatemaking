library(tidyverse)
library(sf)
library(units)
library(viridis)

sf_use_s2(FALSE)

claims <- read_csv("./Data/Belgium/claims_belgium.csv")

pc_map <- st_read("./Data/Belgium/Belgium.shp") %>% st_make_valid()

claims_sp <- claims %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs("WGS84"))

centr <- st_point_on_surface(pc_map)

ggplot() +
  geom_sf(data = pc_map) +
  geom_sf(data = centr, size = .5) +
  theme_bw() +
  ggtitle("Zip Codes and centroids") +
  theme(plot.title = element_text(size = 22))

# Variables Creation ------------------------------------------------------

claims_sp <- claims_sp %>%
  mutate(loss_cost = amount / exposure)

zip_losses <- claims_sp %>%
  st_drop_geometry() %>%
  group_by(pc) %>%
  summarise(
    losses = sum(amount),
    exp = sum(exposure),
    numbers = sum(nclaims)
  ) %>%
  mutate(
    loss_cost = losses / exp,
    freq = numbers / exp,
    sev = if_else(numbers == 0, 0, losses / numbers)
  ) %>%
  left_join(pc_map, by = c("pc" = "POSTCODE")) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = pc_map) +
  geom_sf(data = zip_losses, aes(fill = loss_cost)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Loss Cost") +
  theme(plot.title = element_text(size = 22))

ggplot() +
  geom_sf(data = pc_map) +
  geom_sf(data = zip_losses, aes(fill = freq)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Frequency") +
  theme(plot.title = element_text(size = 22))

ggplot() +
  geom_sf(data = pc_map) +
  geom_sf(data = zip_losses, aes(fill = sev)) +
  scale_fill_viridis(option = "F") +
  theme_bw() +
  ggtitle("Severity") +
  theme(plot.title = element_text(size = 22))
