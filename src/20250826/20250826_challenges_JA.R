# https://hackmd.io/DsuDuIKjS4uPkg8CWQdUAA?view


library(tidyverse)
library(ggspatial)
library(ggrepel)
library(mapview)
library(leafpop)
library(leafem)
library(inbospatial)
# library(sf)  # sf is loaded by ggplot2)
# library(raster)
# library(webshot) required by mapview::mapshot()

# Read the Geopackage with number of Giant Hogweed occurrences per municipality
n_giant_hogweed <- sf::st_read(
  dsn = "data/20250826/20250826_giant_hogweed_per_municipality.gpkg",
  layer = "giant_hogweed_per_municipality"
)

# CHALLENGE 1 ####

## 1.1 ####
map <- mapview::mapView(
  x = n_giant_hogweed,
  zcol = "n",
  legend = TRUE,
  color = "white"
  )
n_giant_hogweed |>
  mapview::mapView(x = _)
n_giant_hogweed |>
  mapview::mapView()
n_giant_hogweed %>%
  mapview::mapView()
n_giant_hogweed %>%
  mapview::mapView(x = .)




## 1.2 & 1.3 ####
map_upd <- mapview::mapView(
  x = n_giant_hogweed,
  zcol = "n",
  legend = TRUE,
  color = "white",
  ## 1.2
  alpha = 0.5,
  alpha.regions = 0.8,
  layer.name = "Number of giant hogweed occurrences"
  ,popup = leafpop::popupTable( # visible upon clicking on the municipalities
    x = n_giant_hogweed,
    zcol = c("mun_name_nl", "mun_name_fr", "n")
  ),
  ## 1.3
  map.types = "OpenStreetMap"
)
map_upd

## 1.4 ####
giant_hogweed <- readr::read_tsv(
  "data/20250826/20250826_giant_hogweed_fl_bxl.tsv",
  na = ""
) %>%
  sf::st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326
  )
map_upd2 <- map_upd + mapview::mapView(
  x = giant_hogweed,
  col.regions = "gray80",
  layer.name = "Giant hogweed occurrences",
  cex = 5
)
# alternative: hand lists to the function arguments
map_upd2

## 1.5 ####
map_upd2 |>
  leafem::addMouseCoordinates()

## 1.6 ####
map_upd2 |>
  mapview::mapshot(
  url = file.path(
    "data",
    "20250826",
    "map_hogweed.html"
  )
)

# CHALLENGE 2 ####

# Get n_giant_hogweed for Brussels only
n_giant_hogweed_bxl <- n_giant_hogweed |>
  dplyr::filter(reg_name_nl == "['Brussels Hoofdstedelijk Gewest']")

n_giant_projected <- n_giant_hogweed_bxl |>
  sf::st_transform(crs = "EPSG:3857")
sf::st_crs(n_giant_projected)$Name

## 2.1 & 2.2 ####
# How to plot the municipalities of Brussels (n_giant_hogweed_bxl) with ggplot?
# Hint: see documentation of the geom_sf function or the general hint below.
map_bxl <- ggplot2::ggplot(
  #data = n_giant_hogweed_bxl
  data = n_giant_projected
  ) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = n),
    color = "red"
  ) +
  ggplot2::scale_fill_viridis_c(direction = 1)
map_bxl

## 2.3 ####
map_bxl + ggplot2::geom_sf_text(
  ggplot2::aes(
    label = mun_name_nl
  ),
  size = 3,
  color = "darkgrey",
  fontface = "bold",
  check_overlap = TRUE
)
# warning "st_point_on_surface may not give correct results for longitude/latitude data"
# has to do with using wgs84

## 2.4 ####


## 2.5 ####



# CHALLENGE 3 ####

## 3.1 ####


## 3.2 ####


## 3.3 ####


## 3.4 ####



# BONUS CHALLENGE ####

## BC.1 ####


## BC.2 ####


