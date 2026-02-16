
# load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(ggrepel)
library(maptiles)
library(stars)
library(tidyterra)
library(terra)
library(basemaps)
library(shadowtext)
library(ggspatial)


# set paths ---------------------------------------------------------------

setwd("~/VietnamBats_Windfarms")
Sys.getenv("MAPBOX_TOKEN")
windowsFonts(Arial = windowsFont("Arial"))
usethis::edit_r_environ()
set_defaults(mapbox = list(access_token = Sys.getenv("MAPBOX_TOKEN")))

# read data ---------------------------------------------------------------

kmz_path <- "data/sites.kmz"
kml_dir <- "data/kmz_extract"
dir_create(kml_dir)

unzip(kmz_path, exdir = kml_dir)

kml_file <- dir_ls(kml_dir, glob = "*.kml")

sites <- st_read(kml_file, quiet = TRUE)


# prep data ---------------------------------------------------------------

sites_3857 <- sites %>% 
  st_make_valid() %>% 
  st_transform(3857)

saltponds_pt_3857 <- st_as_sf(
  tibble(lon = 108.873620, lat = 11.415765),
  coords = c("lon", "lat"),
  crs = 4326) %>% 
  st_transform(3857)

saltponds_xy <- saltponds_pt_3857 %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(label = "Salt\nPonds")

ext <- sites_3857 %>% 
  st_union() %>% 
  st_buffer(1000) %>%
  st_bbox()

ext <- st_bbox(sites_3857)

sat <- basemap_raster(
  ext,
  map_service = "mapbox",
  map_type = "satellite",
  map_token = Sys.getenv("MAPBOX_TOKEN")
)

sat_terra <- rast(sat)   # RasterBrick -> SpatRaster


# data management ---------------------------------------------------------

sites_tagged <- sites_3857 %>% 
  mutate(
    site_type = case_when(
      !is.na(Name) & str_detect(Name, "^WTG") ~ "turbine",
      !is.na(Description) & str_detect(Description, "^WTG") ~ "detector",
      TRUE ~ "other"
    )
  )

turbines  <- filter(sites_tagged, site_type == "turbine")
detectors <- filter(sites_tagged, site_type == "detector")

turbine_labels <- turbines %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  bind_cols(Name = turbines$Name)

# plot --------------------------------------------------------------------


ggplot() +
  geom_spatraster_rgb(data = sat_terra) +
  geom_sf(
    data = detectors,
    shape = 21, fill = "white", color = "black",
    stroke = 1.2, size = 4.5) +
  geom_sf(
    data = detectors,
    shape = 16, color = "black",
    size = 1.6) +
  geom_sf(
    data = turbines,
    shape = 21, fill = "yellow", color = "black",
    stroke = 1.2, size = 4.5) +
  geom_sf(
    data = turbines,
    shape = 16, color = "black",
    size = 1.6) +
  shadowtext::geom_shadowtext(
    data = turbine_labels,
    aes(X, Y, label = Name),
    family = "Arial", colour = "white", bg.colour = "black",
    bg.r = 0.18,     # halo thickness
    size = 3, nudge_y = -220) +
  coord_sf(crs = 3857, expand = FALSE) +
  theme_void() +
  shadowtext::geom_shadowtext(
    data = saltponds_xy,
    aes(X, Y, label = label),
    inherit.aes = FALSE,
    family = "Arial",
    colour = "white",
    bg.colour = "black",
    bg.r = 0.18,     # halo thickness
    size = 3,
    lineheight = 0.9,
    hjust = 0.5,
    vjust = 0.5) +
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


