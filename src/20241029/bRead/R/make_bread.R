#' Make bread from grains, yeast, water and salt
#'
#' @param grains A scalar representing the amount of grains.
#' @param yeast A scalar representing the amount of yeast.
#' @param water A scalar representing the amount of water.
#' @param salt A scalar representing the amount of salt.
#'
#' @return A scalar representing the amount of bread.
#' @export
#'
#' @examples
#' make_bread(grains = 1, yeast = 1, water = 1.5, salt = 3)
#'
make_bread <- function(grains, yeast, water, salt) {
  # Code to generate `bread`
  assertthat::is.scalar(grains)
  assertthat::is.scalar(yeast)
  assertthat::is.scalar(water)
  assertthat::is.scalar(salt)
  bread <- grains + yeast + water + salt
  return(bread)
}
