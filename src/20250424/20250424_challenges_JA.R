library(sf)
library(tidyverse)
library(mapview) # Optional


# ------ challenge 1 --------------------

heracleum_df <- readr::read_tsv(
  "data/20250424/20250424_heracleum_BE.tsv"
)
str(heracleum_df)

heracleum <- sf::st_as_sf(
  x = heracleum_df,
  # first x then y: 1: Longitude, 2: Latitute
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)
str(heracleum)
sf::st_crs(heracleum)


municipalities <- sf::st_read(
  dsn = "data/20250424/20250424_communesgemeente-belgium/georef-belgium-municipality-millesime.shp"
)
# View(municipalities)
# just a data frame, with column 'geometry'

# read in layers from protected areas
layers <- sf::st_layers("data/20250424/20250424_protected_areas_BE.gpkg")
layers
pa <- sf::st_read(
  dsn = "data/20250424/20250424_protected_areas_BE.gpkg",
  layer = layers[[1]][1]
)
# crs is projected; values are in meters

pa_bioregion <- sf::st_read(
  dsn = "data/20250424/20250424_protected_areas_BE.gpkg",
  layer = layers[[1]][2]
)
pa_habitats <- sf::st_read(
  dsn = "data/20250424/20250424_protected_areas_BE.gpkg",
  layer = layers[[1]][3]
)

# retrieve crs
sf::st_crs(pa) # important information: ID
sf::st_crs(heracleum)
sf::st_crs(municipalities)

# extract protected areas of sitetype A
pa_a <- pa |>
  dplyr::filter(
    SITETYPE == "A"
  )

mapview::mapview(
  heracleum
)


# ------ challenge 2 --------------------


pa_a_wgs84 <- pa_a |>
  st_transform(
    x = _,
    crs = 4326
  )
# mapview: transformation can look different on maps because of distortions


# sf::st_write()

heracleum_circles <- heracleum |>
  dplyr::mutate(
    geometry_circles = sf::st_buffer(
      geometry,
      dist = coordinateUncertaintyInMeters,
      nQuadSegs = 10
    )
  )
# buffer can also be added to WGS 84 and then project (apparently faster)
# but then no control over shape of buffer apparently, for that first project
heracleum_circles <- heracleum |>
  sf::st_transform(
    x = _,
    crs = 31370
  ) |>
  dplyr::mutate(
    geometry = sf::st_buffer(
      geometry,
      dist = coordinateUncertaintyInMeters,
      nQuadSegs = 3
    )
  )
mapview::mapview(heracleum_circles)


# ------ challenge 3 --------------------

# package waldo for comparing objects (sf but in general)
