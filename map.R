
# load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(rstac)
library(terra)
library(shadowtext)
library(ggspatial)


# set paths ---------------------------------------------------------------

setwd("~/VietnamBats_Windfarms")
windowsFonts(Arial = windowsFont("Arial"))


# load data ---------------------------------------------------------------

kmz_path <- "data/sites.kmz"
kml_dir <- "data/kmz_extract"
 
read_kmz <- function(path, out_dir = "data/kmz_extract") {
  dir.create(out_dir, showWarnings = FALSE)
  unzip(path, exdir = out_dir)
  kml <- list.files(out_dir, pattern = "\\.kml$", full.names = TRUE)[1]
  sf::st_read(kml, quiet = TRUE) 
}

sites <- read_kmz("data/sites.kmz")

# get raster --------------------------------------------------------------

bb <- st_bbox(st_transform(sites, 4326))

items <- stac("https://earth-search.aws.element84.com/v1") %>% 
  stac_search(
    collections = "sentinel-2-l2a",
    bbox = c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]),
    datetime = "2025-01-01T00:00:00Z/2025-12-31T23:59:59Z"
    ) %>% 
  get_request()

# pick least cloudy
cc <- vapply(items$features, function(x) x$properties[["eo:cloud_cover"]], numeric(1))
it <- items$features[[which.min(cc)]]

# read RGB bands (10m)
r <- rast(it$assets$red$href)
g <- rast(it$assets$green$href)
b <- rast(it$assets$blue$href)

rgb <- c(r, g, b)

aoi_utm <- sites %>% 
  st_union() %>% 
  st_buffer(2000) %>% 
  st_transform(st_crs(crs(rgb)))

rgb_crop <- crop(rgb, aoi_utm)

# stretch each band using quantiles -> 8-bit 0..255
rgb8 <- terra::stretch(rgb_crop, minq = 0.02, maxq = 0.98)
 
# scale to 0..1 (ggplot-friendly)
rgb01 <- rgb8 / 255

sites_rgb <- st_transform(
  sites, 
  st_crs(crs(rgb01)))

ext_small <- ext(
  265960,
  273260,
  1256474,
  1265873)
 
rgb_small <- crop(rgb01, ext_small)

# create points and labels ------------------------------------------------

saltponds_sf <- st_as_sf(
    tibble(lon = 108.873620, lat = 11.415765),
    coords = c("lon", "lat"),
    crs = 4326
    ) %>% 
  st_transform(st_crs(crs(rgb01)))
 
saltponds_xy <- saltponds_sf %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(label = "Salt\nPonds")
 
solar_parks_ll <- tibble(
    group = c("A","A","B","B"),
    park  = c("A1","A2","B1","B2"),
    lon   = c(108.8800183, 108.861204, 108.871763, 108.873842),
    lat   = c(11.396708, 11.407223, 11.437577, 11.443189)
    )
 
solar_labels_ll <- tibble(
    group = c("A","B"),
    lon   = c(108.868789, 108.864832),
    lat   = c(11.397725, 11.440588),
    label = "Solar\nParks"
    )
target_crs <- st_crs(crs(rgb01))
 
solar_parks_sf <- st_as_sf(solar_parks_ll, coords = c("lon","lat"), crs = 4326) |>
  st_transform(target_crs)

solar_labels_sf <- st_as_sf(solar_labels_ll, coords = c("lon","lat"), crs = 4326) |>
  st_transform(target_crs)
solar_parks_xy <- solar_parks_sf %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  bind_cols(solar_parks_ll |> select(group, park))

solar_labels_xy <- solar_labels_sf %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  bind_cols(solar_labels_ll |> select(group, label))

lines_df <- solar_parks_xy %>% 
  left_join(
    solar_labels_xy %>%  select(group, x_lab = X, y_lab = Y),
    by = "group"
    ) %>% 
  transmute(
    x    = x_lab,
    y    = y_lab,
    xend = X,
    yend = Y
    )

sites_tagged <- sites_rgb %>% 
  mutate(
    site_type = case_when(
      !is.na(Name) & str_detect(Name, "^WTG") ~ "turbine",
      !is.na(Description) & str_detect(Description, "^WTG") ~ "detector",
      TRUE ~ "other"))

turbines  <- filter(sites_tagged, site_type == "turbine")
detectors <- filter(sites_tagged, site_type == "detector")
 
turbine_labels <- turbines %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      bind_cols(Name = turbines$Name)


# plot --------------------------------------------------------------------

wind.farm.map <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = rgb_small,
    r = 1, g = 2, b = 3,
    stretch = "none",
    max_col_value = 1
    ) +
  geom_segment(
    data = lines_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color = "white",
    linewidth = 0.6,
    lineend = "round"
    ) +
  shadowtext::geom_shadowtext(
    data = solar_labels_xy,
    aes(X, Y, label = label),
    inherit.aes = FALSE,
    family = "Arial",
    colour = "white",
    bg.colour = "black",
    bg.r = 0.18,
    size = 3.2,
    lineheight = 0.9
   ) +
  geom_sf(
    data = detectors,
    shape = 21, fill = "white", color = "black",
    stroke = 1.2, size = 3) +
  geom_sf(
    data = detectors,
    shape = 16, color = "black",
    size = 1) +
  geom_sf(
    data = turbines,
    shape = 21, fill = "yellow", color = "black",
    stroke = 1.2, size = 3) +
  geom_sf(
    data = turbines,
    shape = 16, color = "black",
    size = 1) +
  shadowtext::geom_shadowtext(
    data = turbine_labels,
    aes(X, Y, label = Name),
    family = "Arial", colour = "white", bg.colour = "black",
    bg.r = 0.18,     # halo thickness
    size = 3, nudge_y = -220) +
  shadowtext::geom_shadowtext(
    data = saltponds_xy,
    aes(X, Y, label = label),
    inherit.aes = FALSE,
    family = "Arial",
    colour = "white",
    bg.colour = "black",
    bg.r = 0.18,
    size = 3,
    lineheight = 0.9,
    hjust = 0.5,
    vjust = 0.5) +
  coord_sf(crs = st_crs(crs(rgb01)), expand = FALSE) +
  theme_void() +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      text_family = "Arial",
      text_col = "white",
      line_col = "white"
      ),
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm"),
    pad_x  = unit(0.3, "cm"),
    pad_y  = unit(0.3, "cm")
    ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.4,
    unit_category = "metric",
    text_family = "Arial",
    text_col = "white",
    pad_x = unit(0.3, "cm"),
    pad_y = unit(0.3, "cm")
    )




ggsave(
  +   filename = "figs/map.png",
  +   plot = wind.farm.map,                 # or last_plot()
  +   width = 5.45,
  +   height = 7.5,
  +   units = "in",
  +   dpi = 600,
  +   bg = "white"
  + )