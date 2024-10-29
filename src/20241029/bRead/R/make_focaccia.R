#' Make focaccia from grains, yeast, water and salt
#'
#' @param grains A scalar representing the amount of grains.
#' @param yeast A scalar representing the amount of yeast.
#' @param water A scalar representing the amount of water.
#' @param salt A scalar representing the amount of salt.
#'
#' @return A scalar representing the amount of focaccia.
#' @export
#'
#' @examples
#' make_focaccia(grains = 1, yeast = 1, water = 1.5, salt = 3)
#'
make_focaccia <- function(grains, yeast, water, salt) {
  # Code to generate `focaccia`
  assertthat::is.scalar(grains)
  assertthat::is.scalar(yeast)
  assertthat::is.scalar(water)
  assertthat::is.scalar(salt)
  focaccia <- grains + 1.5 * yeast + 0.7 * water + 2 * salt
  return(focaccia)
}
