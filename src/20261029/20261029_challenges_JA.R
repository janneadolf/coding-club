library(tidyverse)
library(sf) # to deal with spatial vector data
library(qgisprocess)
remotes::install_github("JanCaha/r_package_qgis")
library(terra) # to deal with raster data
library(mapview) # optional: to easily create maps for visualization purposes


mapviewOptions(basemaps = c("OpenStreetMap"))

# Challenge 0 -------------------------------------------------------------

# Get all QGIS algorithm providers
qgis_providers()

# Read input data from sf package
input <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
plot(sf::st_geometry(input)) # Not present in example online

# Run the QGIS algorithm "native:buffer" with the input data, a distance of 1,
# and dissolve set to TRUE
result <- qgis_run_algorithm(
  "native:buffer",
  INPUT = input,
  DISTANCE = 1,
  DISSOLVE = TRUE
)

result

output_sf <- sf::st_as_sf(result)
plot(sf::st_geometry(output_sf))

mapview::mapview(output_sf) + mapview::mapview(input)

# CHALLENGE 1 ####
# Read `20261029_watersurfaces_hab.gpkg` using sf package
water <- sf::st_read(
  "./data/20261029/20261029_watersurfaces_hab.gpkg",
  layer = "watersurfaces_hab_polygons"
)

# Show water with mapview
mapview::mapview(water)

# Read `20261029_habitatsprings.geojson`
hab_springs <- sf::st_read(
  "./data/20261029/20261029_habitatsprings_flanders.geojson"
)

mapview::mapview(hab_springs)

## 2.2 & 2.3 ----
qgis_search_algorithms('shortest') |> View()

qgis_show_help(algorithm = "native:shortestline")

help <- qgis_get_argument_specs(algorithm = "native:shortestline")
help |> View()

result2 <- qgis_run_algorithm(
  "native:shortestline",
  #SOURCE = hab_springs,
  #DESTINATION = water
  SOURCE = water,
  DESTINATION = hab_springs
)

output2_sf <- sf::st_as_sf(result2)

mapview::mapview(output1_sf) +
  mapview::mapview(water) +
  mapview::mapview(hab_springs)

## 2.4 ----
output_sf <- input |>
  qgis_run_algorithm_p(
    .data = _, # is probably the first argument provided to the 'native:buffer' algorithm
    algorithm = "native:buffer",
    DISTANCE = 1,
    DISSOLVE = TRUE
  ) |>
  sf::st_as_sf()

mapview::mapview(output_sf) + mapview::mapview(input)


buffer_fun <- qgis_function("native:buffer")
output_sf <- input |>
  buffer_fun(
    #INPUT = _,
    DISTANCE = 1,
    DISSOLVE = TRUE
  ) |>
  sf::st_as_sf()

output_sf <- 1 |>
  buffer_fun(
    INPUT = input,
    DISTANCE = _,
    DISSOLVE = TRUE
  ) |>
  sf::st_as_sf()

mapview::mapview(output_sf) + mapview::mapview(input)


# Intermezzo 1 ------------------------------------------------------------

library(tictoc) # install it first, if needed

tic("buffer via sf")
buffer_via_sf <- sf::st_buffer(water, nQuadSegs = 5000, dist = 100)
toc()

tic("buffer via qgisprocess")
buffer_via_qgisprocess <- qgis_run_algorithm(
  INPUT = water,
  algorithm = "native:buffer",
  SEGMENTS = 5000,
  DISTANCE = 100
)
toc()


# Challenge 2 -------------------------------------------------------------

hydrocotyle_ranunculoides <- sf::st_read(
  "./data/20261029/20261029_hydrocotyle_ranunculoides.geojson"
)

prot_areas <- sf::st_read(
  "./data/20261029/20261029_natura2000_protected_areas.gpkg"
)

mapview(hydrocotyle_ranunculoides)
mapview(prot_areas)

## 2.1 ----

qgis_search_algorithms('count') |> View()

qgis_show_help(algorithm = "native:countpointsinpolygon")

help <- qgis_get_argument_specs(algorithm = "native:countpointsinpolygon")
help |> View()

counts <- water |>
  qgis_run_algorithm_p(
    .data = _,
    algorithm = "native:countpointsinpolygon",
    # POLYGONS = water,
    POINTS = hydrocotyle_ranunculoides
  ) |>
  sf::st_as_sf() #|>
#dplyr::pull(NUMPOINTS)
# this just gets added to data frame; name see documentation
# see documentation in algorithm

## 2.2 ----

buffer <- qgis_run_algorithm(
  "native:buffer",
  INPUT = hab_springs,
  DISTANCE = 'expression: "area_m2" * 3.14' # no double quotes allowed!
) |>
  st_as_sf()

mapview::mapview(hab_springs) +
  mapview::mapview(buffer, col.regions = "red")


## 2.3 ----

qgis_search_algorithms('validity') |> View()
qgis_show_help(algorithm = "native:checkvalidity")
help <- qgis_get_argument_specs(algorithm = "native:checkvalidity")
help |> View()


## 2.4 ----
invalid <- sf::st_sf(
  case = c(
    "bowtie",
    "self-touching ring",
    "hole outside shell",
    "duplicate point"
  ),
  geometry = sf::st_as_sfc(c(
    # bowtie: the ring crosses itself
    "POLYGON((0 0, 2 2, 2 0, 0 2, 0 0))",
    # ring touches itself in a single point
    "POLYGON((5 0, 8 0, 8 3, 6 1, 5 3, 5 0, 7 1, 5 0))",
    # hole lies completely outside the outer ring
    "POLYGON((10 0, 14 0, 14 4, 10 4, 10 0), (20 20, 21 20, 21 21, 20 21, 20 20))",
    # repeated vertex (usually still valid, a good contrast case)
    "POLYGON((0 10, 4 10, 4 10, 4 14, 0 14, 0 10))"
  )),
  crs = 31370
)

plot(invalid, col = c("red", "orange", "yellow", "green"), border = "black")
sf::st_is_valid(invalid)
# Explains why each one is invalid
sf::st_is_valid(invalid, reason = TRUE)


# INTERMEZZO 2 ####
input <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

# Using qgisprocess
input %>%
  qgisprocess::qgis_run_algorithm_p(
    algorithm = "native:buffer",
    DISTANCE = 0.2,
    DISSOLVE = TRUE,
    END_CAP_STYLE = "Flat"
  ) %>%
  sf::st_as_sf() %>%
  plot()

# Using qgis R package
input %>%
  qgis::qgis_buffer(
    DISTANCE = 0.2,
    DISSOLVE = TRUE,
    END_CAP_STYLE = "Flat"
  ) %>%
  st_as_sf() %>%
  plot()

input %>%
  qgis::qgis_buffer(
    INPUT = .,
    DISTANCE = 0.2,
    DISSOLVE = TRUE,
    END_CAP_STYLE = "Flat"
  ) %>%
  st_as_sf() %>%
  plot()


# Challenge 3A ------------------------------------------------------------

# Challenge 3B ------------------------------------------------------------

land_use <- terra::rast("./data/20231214/20231214_land_use_nara_2016_100m.tif")
land_use


## 3B.2 ####
de_maten <- prot_areas %>% dplyr::filter(NAAM == "De Maten")
