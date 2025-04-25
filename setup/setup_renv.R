# create renv ignore
dirnames <- list.dirs("src", full.names = FALSE) |> tail(-1)
dirnames_num <- gsub("\\_.*", "", dirnames) |> as.numeric()
startdate <- "20250130"
dirnames_ignore <- dirnames[dirnames_num < startdate] |>
paste0("src/" , ... = _)
readr::write_lines(dirnames_ignore, file = ".renvignore")

# activate renv
renv::init()
