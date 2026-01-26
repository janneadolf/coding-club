install.packages(c("sf", "readr", "mapview"))

library(sf)
library(readr)
library(dplyr)
library(mapview) # for basic visualization

# Read occurrences
occs_df <- readr::read_tsv(
  "data/20260126/20260126_occurrences_raccoon_be.txt",
  na = "",
  guess_max = 1000000
)

# Read species occurrence cube
cube <- readr::read_tsv(
  "data/20260126/20260126_species_cube_raccoon_be.csv",
  na = ""
)

# Unzip municipalities shapefile
zip_file <- "data/20260126/20260126_communesgemeente-belgium.zip"
municipalities_folder <- "data/20260126/20260126_municipalities_belgium"
unzip(zip_file, exdir = municipalities_folder)

# Unzip EEA grid shapefile
zip_file <- "data/20260126/20260126_eea_grid_be.zip"
eea_grid_folder <- "data/20260126/20260126_eea_grid_be"
unzip(zip_file, exdir = eea_grid_folder)


# ---- challenge 1 ----------------------------------------------------------------

# challenge 1.1
occs <- st_as_sf(
  x = occs_df,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
  )
mapview(occs)

# challenge 1.2
st_layers(dsn = "data/20260126/20260126_protected_areas_be.gpkg")

# challenge 1.3
pa <- st_read(
  dsn = "data/20260126/20260126_protected_areas_be.gpkg",
  layer = "NaturaSite_polygon"
)

# challenge 1.4
pa_bioregion <- st_read(
  "data/20260126/20260126_protected_areas_be.gpkg",
  layer = "BIOREGION"
)
pa_habitat <- st_read(
  "data/20260126/20260126_protected_areas_be.gpkg",
  layer = "HABITATS"
)

# challenge 1.5
municipalities_old <- sf::st_read(
  "data/20260126/20260126_municipalities_belgium/georef-belgium-municipality-millesime.shp"
)
municipalities <- sf::st_read(
  municipalities_folder,
  layer = "georef-belgium-municipality-millesime" # good practice: specify layer
  )
waldo::compare(municipalities_old, municipalities) # no diff

mapview(municipalities)

# challenge 1.6
eea_grid <- sf::st_read(
  eea_grid_folder
)

# challenge 1.7
st_crs(occs)$Name
st_crs(pa)$Name
st_crs(municipalities)$Name
st_crs(eea_grid)$Name

# challenge 1.8
pa_a <- pa |>
  dplyr::filter(SITETYPE == "A")


# ---- challenge 2 ----------------------------------------------------------------

# challenge 2.1
occs_3035 <- occs |>
  st_transform(crs = 3035)
st_crs(occs_3035)$Name

# challenge 2.2
# https://r-spatial.github.io/sf/articles/sf2.html#using-st_write
st_write(pa,
         layer = "NaturaSite",
         "data/20260126/pa_occs_3035.gpkg")
st_write(occs_3035,
         layer = "racoon_occs",
         "data/20260126/pa_occs_3035.gpkg")
st_layers("data/20260126/pa_occs_3035.gpkg")

# challenge 2.2
municipalities_3035 <- municipalities |>
  st_transform(crs = 3035)

# challenge 2.3
municipalities_3035_centroids <- municipalities_3035 |>
  st_centroid()

mapview(
  list(municipalities_3035, municipalities_3035_centroids),
  col.regions = c("blue", "red")
  )


# ---- challenge 3 ----------------------------------------------------------------

colnames(cube)
colnames(eea_grid)

# challenge 3.1
cube_sf <- left_join(
  x = cube,
  y = eea_grid,
  by = "CELLCODE"
) |>
  st_as_sf()


# challenge 3.2
inter <- st_intersects(
  municipalities_3035,
  pa
  )
nrow(municipalities_3035) == length(inter)

municipalities_3035 <- municipalities_3035 |>
  dplyr::mutate(
    n_pa = purrr::map(
      inter,
      \(x) { length(x) }
    )
  )

# challenge 3.3
no_pa_municipalities_3035 <- municipalities_3035 |>
  dplyr::filter(
    n_pa == 0
  )
nrow(no_pa_municipalities_3035)

# challenge 3.4
no_pa_municipalities_3035 |>
  dplyr::mutate(
    area = st_area(no_pa_municipalities_3035)
  ) |>
  dplyr::arrange(
    area |> dplyr::desc()
  ) |>
  dplyr::slice_head(n = 10) |>
  dplyr::select("mun_name_up")

# challenge 3.5
