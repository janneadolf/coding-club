library(tidyverse)
library(rgbif)

# Load data

butterflies_eu <- readr::read_csv(
  file = "data/20250130/20250130_butterflies_eu.csv",
  na = ""
)

butterflies_eu_distr_all <- readr::read_csv(
  file = "data/20250130/20250130_butterflies_eu_distributions.csv",
  na = ""
)

butterflies_eu_vern_dutch <- readr::read_csv(
  file = "data/20250130/20250130_butterflies_eu_vernacularnames_dutch_all_GBIF.csv",
  guess_max = 1000
)

transect_counts <- readr::read_csv(
  "data/20250130/20250130_transect_counts.csv",
  na = ""
)


## ----------- CHALLENGE 1 - basics -----------------------------

#Get the unique values of column threatStatus in butterflies_eu_distr.
#(Bonus: both as vector (basic R) or data.frame with 1 one column (dplyr)).

threatstatus_names <- butterflies_eu_distr_all$threatStatus |>
  unique()

threatstatus_names <- butterflies_eu_distr_all |>
  dplyr::select(threatStatus) |>
  dplyr::distinct(threatStatus) |>
  as.data.frame()

# Remove the rows without country, i.e. the data at European level.
# Save it as butterflies_eu_distr.

butterflies_eu_distr <- butterflies_eu_distr_all |>
  dplyr::filter(!is.na(country))

butterflies_eu_distr <- butterflies_eu_distr_all |>
  tidyr::drop_na(country)

# Count the number of taxon keys (column taxonKey) for each threatStatus.

butterflies_eu_distr_sum <- butterflies_eu_distr |>
  dplyr::group_by(threatStatus) |>
  #dplyr::distinct(taxonKey) |>
  dplyr::summarise(n_species = n()) |>
  dplyr::ungroup()

butterflies_eu_distr_sum <- butterflies_eu_distr |>
  dplyr::count(threatStatus)

# Count the number of taxon keys (column taxonKey) for each threatStatus and country.

butterflies_eu_distr_sum <- butterflies_eu_distr |>
  dplyr::count(threatStatus, country)

# Order the count of taxon keys (column taxonKey) for each threatStatus and country in descending order.

butterflies_eu_distr |>
  dplyr::count(threatStatus, country) |>
  dplyr::arrange(n |> desc())


# Remove columns source and remarks from butterflies_eu_distr.

butterflies_eu_distr_upd <- butterflies_eu_distr |>
  dplyr::select(!c(source, remarks))

# Move column country between taxonKey and locality. Move threatStatus after locationId.

butterflies_eu_distr_upd <- butterflies_eu_distr |>
  dplyr::relocate(country, .after = taxonKey) |>
  dplyr::relocate(threatStatus, .after = locationId)

# Optional. Sometimes we cannot know in advance that the columns we want to select/remove from a data.frame exist. For example, how to remove source and remarks without returning an error if they do not exist?

butterflies_eu_distr_upd <- butterflies_eu_distr |>
  dplyr::select(!tidyselect::any_of(c("source", "remarks")))



## ----------- CHALLENGE 2 - To join or not to join 🦋 -----------------------------

#Filter butterflies_eu to get only the Belgian butterflies (country code: BE in butterflies_eu_distr). Save the result as butterflies_be. Important: no need to join the two datasets!

keys_BE <- butterflies_eu_distr |>
  dplyr::filter(grepl("BE", country)) |>
  dplyr::pull(taxonKey)

butterflies_be <- butterflies_eu |>
  dplyr::filter(key %in% keys_BE)

#Some countries haven't reported the threatStatus (threatStatus = NA) for any species in butterflies_eu_distr. Which ones? Important: many countries reported the threatStatus for some species, but not for all of them and should be excluded.

butterflies_eu_distr_upd <- butterflies_eu_distr |>
  dplyr::mutate(
    n_rows = dplyr::n(),
    n_rows_na = is.na(threatStatus) |> sum(),
    .by = country
  ) |>
  dplyr::filter(n_rows == n_rows_na)

# The transect_counts dataset contains the counts of butterflies in a transect.
# The column species contains the Dutch vernacular names of the butterflies.
# Add to transect_counts a column called scientificName with the scientific name contained in butterflies_be (column scientificName). You can do this by matching the vernacular names in transect_counts$species with the vernacular names contained in butterflies_eu_vern_dutch$vernacularName. Tip 1: use butterflies_eu_vern_dutch$taxonKey and butterflies_be$nubKey for comparing/joining. Tip 2: comparing the lower case version of the vernacular names can be useful to avoid mismatches due to capitalization.


transect_counts_upd <- transect_counts |>
  dplyr::mutate(species = species |>tolower())

butterflies_eu_vern_dutch_upd <- butterflies_eu_vern_dutch |>
  dplyr::mutate(vernacularName = vernacularName |> tolower()) |>
  dplyr::distinct(vernacularName, .keep_all = TRUE)

butterflies_joined <- dplyr::left_join(
  x = transect_counts_upd,
  y =  butterflies_eu_vern_dutch_upd,
  by = c("species" = "vernacularName")
) |>
  dplyr::left_join(
    x = _,
    y = butterflies_be,
    by = c("taxonKey" = "nubKey")
  )

## INTERMEZZO: rgbif + purrr = 💪

# It's almost impossible to do data wrangling with tidyverse without using purrr. purrr is a package from tidyverse ecosystem. It allows you to write code in a more functional style, which can be more readable and maintainable. rgbif is an R package to interface with the Global Biodiversity Information Facility (GBIF) API. It allows you to search for species, download occurrence data, and more. purrr and rgbif can be combined to make powerful and flexible workflows for working with biodiversity data. Here are some examples of how you can use purrr and rgbif together to get vernacular names.

# The "National checklists and red lists for European butterflies" dataset contains only vernacular names in English. Just check it via:

purrr::map(
  butterflies_eu$key, function(x) {
    rgbif::name_usage(key = x, data = "vernacularNames") %>%
      purrr::pluck("data")
  }) %>%
  purrr::list_rbind() %>%
  dplyr::distinct(language)

# We are interested in the Dutch vernacular names. How to get them? Let's do it by using the GBIF Taxonomic Backbone! The field `nubKey` in `butterflies_eu` contains the key of the taxon in the GBIF Taxonomic Backbone. We can use it to get the Dutch vernacular names of the Belgian butterflies:

butterflies_eu_vern_dutch <- purrr::map(
  butterflies_eu$nubKey, function(x) {
    rgbif::name_usage(key = x, data = "vernacularNames") %>%
      purrr::pluck("data")
  }) %>%
  purrr::list_rbind() %>%
  dplyr::filter(language == "nld")


## CHALLENGE 3 - So many names, so little time 🦋

#The data.frame butterflies_eu_vern_dutch contains several vernacular names for the same species (taxonKey). Some of them are exactly the same: they are just collected from different checklists. How many unique vernacular names are there for each taxon?

butterflies_eu_vern_dutch_sum <- butterflies_eu_vern_dutch |>
  dplyr::group_by(taxonKey) |>
  dplyr::mutate(vernacularName = vernacularName |> tolower()) |>
  dplyr::distinct(vernacularName) |>
  dplyr::summarise(n = n())


#We can see that some unique vernacular names are just the same name written in different ways. For example, "Atalanta" and "atalanta". How many unique vernacular names are there for each taxon if we ignore the case/capitalization? Return a data.frame with two columns: taxonKey and n. Order the result by n, from high to low. Tip: stringr package can be useful.

butterflies_eu_vern_dutch_sum <- butterflies_eu_vern_dutch_sum |>
  dplyr::arrange(n)

#The previous result means that we need to make some choices: vernacular names are not only written in different ways, but they ARE sometimes really different! Example: "akkerparelmoervlinder" and "paarse parelmoervlinder". Before making a choice, let's analyse the situation. Instead of counting only, return a data.frame with three columns: taxonKey, vernacularName_lower (lowercase version of vernacularName) and n_names (number of vernacular names). Again, order the output by n_names, from high to low.

butterflies_eu_vern_dutch_sum <- butterflies_eu_vern_dutch |>
  dplyr::group_by(taxonKey) |>
  dplyr::mutate(vernacularName = vernacularName |> tolower()) |>
  dplyr::distinct(vernacularName) |>
  dplyr::mutate(n = n()) |>
  dplyr() # here



## BONUS CHALLENGE 1 - Choices 🤷






## BONUS CHALLENGE 2 - Tidying up your data 🧹

library(readxl)

counts_raw <- read_excel(
  path = "data/20250130/20250130_butterfly_transect_counts_raw.xls",
  skip = 4,
  .name_repair = "universal"
)
