library(tidyverse)
library(rgbif)

# CHALLENGE 0 ####

height_giraffe <- runif(n = 5, min = 4.3, max = 5.7)
floor(height_giraffe)

# height of 5 giraffes
height_giraffe <- runif(n = 5, min = 4.3, max = 5.7)
# weight of 5 giraffes
weight_giraffe <- rnorm(n = 5, mean = 1192, sd = 300)
# data.frame with height and weight
df_giraffe <- tibble(height = height_giraffe,
                     weight = weight_giraffe)

View(df_giraffe)
colMeans(df_giraffe)

# for each element of the data frame (=column) apply function median
# data frame is list of columns
map(df_giraffe, median) # return list
map_dbl(df_giraffe, median) # return double vector

# more complex example
height_giraffe <- runif(n = 6, min = 4.3, max = 5.7)
# Weight of 5 giraffes
weight_giraffe <- rnorm(n = 6, mean = 1192, sd = 300)
# GPS tracker ID
gps_tracker_giraffe <- c("B0G", "TRT", "4FB", "U7H", "31L", "O9Q")
# Giraffes nicknames
nicknames_giraffe <- sample(
  c("Oberon", "Raïsa", "Dirk", "Emma", "Damiano", "Rhea")
)
# List with weight, height and GPS tracker IDs. Use nicknames as list element names.
giraffes <- tibble::tibble("weight" = weight_giraffe,
                           "height" = height_giraffe,
                           "gpsID" = gps_tracker_giraffe) %>%
  transpose %>%
  set_names(nicknames_giraffe)
bmi <- function(weight, height) weight / height^2

# start from recipie for one element, then use function in map



# CHALLENGE 1 ####


species_df <- dplyr::tibble(
  species = c("Procyon lotor", "Nasua narica", "Muntiacus reevesis"),
  taxonKey = c(5218786, 2433536, 2440946)
)

# Get occurrence data for the given species in the
# Netherlands (country code "NL")
data_nl <- rgbif::occ_search(
  taxonKey = species_df$taxonKey,
  country = "NL",
  year = "1950,2025",
  hasCoordinate = TRUE,
  occurrenceStatus = "PRESENT",
  limit = 100000 # High enough
)
data_nl$`5218786`$data
data_nl$`2433536`$data
data_nl$`2440946`$data

#
# The code provided creates a data frame, species, with 3 species and a nested list, data_nl, with data from GBIF
# related to those species in The Netherlands. The occurrences are in the slot data. See code.


## 1.1 ####
#
# Use species column as names for data_nl elements instead of taxonKey. Hint: you can use a purrr function for it,
# but there could be other ways to solve it.

index <- match(names(data_nl), species_df$taxonKey)
names(data_nl) <- species_df$species[index]


## 1.2 ####
# How many records for each species of data_nl? Return it as a list.

tmp <- map(data_nl, length)
str(tmp)

## 1.3 ####
# How to return it as a (named) vector of integers?

tmp <- map_int(data_nl, length)
str(tmp)

## 1.4 ####
#   Bundle the data slots into a data.frame. Do not hard code it, use purrr.

tmp <- map(data_nl, \(x){
  x$data
}) |> bind_rows()

## 1.5 ####
# It's time to apply same rgbif call for more countries. The vector with countries is provided.
# Create a named list, data_countries, with GBIF data for the 3 species in each country.
# In other words, data_countries[[1]] should be equal to the provided data_nl at the begin of this challenge.
countries <- c("NL", "AT", "ES", "DK")

data_multi <- map(countries, \(x){
  rgbif::occ_search(
    taxonKey = species_df$taxonKey,
    country = x,
    year = "1950,2025",
    hasCoordinate = TRUE,
    occurrenceStatus = "PRESENT",
    limit = 100000 # High enough
  )
})

data_test <- map(countries,
                 rgbif::occ_search,
                 taxonKey = species_df$taxonKey,
                 year = "1950,2025",
                 hasCoordinate = TRUE,
                 occurrenceStatus = "PRESENT",
                 limit = 100000 # High enough
                 )

waldo::compare(data_multi, data_test)


## 1.6 ####
# Why doesn't the returned list have names? How to set countries as names?
# And how to have species instead of taxonKey as names of the nested lists (similar to 1).
# You should end up with something like this, for example: data_countries$NL$Procyon lotor.

names(data_multi) <- countries

# rename 2nd level
data_multi <- map(data_multi, \(x){
  setNames(x, species_df$species)
})
data_multi <- map(data_multi, \(x){
  names(x) <- species_df$species
  x # requires returning this otherwise you return the names
})






# CHALLENGE 2 ####

## 2.1 ####
# How many records are there for each species in each country?
# Return it as a list of countries where each element is a named vector of integers.

tmp <- map(data_multi,
           \(x){
             map_int(.x = x, \(y){
               n_row <- y$data |> nrow()
               if(is.null(n_row)){
                 0
               } else {
                 n_row
               }
             })
           })
tmp
# even better, get to the cause
tmp <- map(data_multi,
\(x){
  map_int(.x = x, \(y){
    if(is.null(y$data)){
      0
    } else {
      nrow(y$data)
    }
  })
})


## 2.2 ####
# How many records in total?
 tmp |> bind_rows() |> colSums()


## 2.3 ####
ggplot2::ggplot(
  data = data_multi$NL$`Procyon lotor`$data
) +
  ggplot2::geom_bar(ggplot2::aes(x = year)) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Number of records") +
  ggplot2::ggtitle("Procyon lotor - NL")


## 2.4 ####


## 2.5 ####



# INTERMEZZO ####


a <- list(z = 1, b = NULL, c = 3)
purrr::compact(a)

purrr::list_c(a)



# CHALLENGE 3 ####

data_countries_df <- readr::read_csv(
  "./data/20250925/20250925_gbif_occs.csv",
  na = ""
)

data_nested <- data_countries_df %>%
  dplyr::group_by(species) %>%
  tidyr::nest()
data_nested

## 3.1 ####
ggplot2::ggplot(data_nested$data[[1]]) +
ggplot2::geom_bar(ggplot2::aes(x = year, fill = country)) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Number of records")


##3.2 ####


