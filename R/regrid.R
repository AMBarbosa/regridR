#' Re-grid raster layer(s) onto an equal-area square-cell grid
#'
#' @description
#' This function extracts the values of a set of raster layers onto a regular
#' equal-area vector polygon grid, then rasterizes the values onto a layer with
#' the same extent, resolution and coordinate reference system as the input
#' vector grid.
#'
#' @param layers A SpatRaster containing the input layer(s) to be re-gridded.
#' @param grid A polygon SpatVector defining the target grid. Must consist
#' of square equal-area cells (e.g. the EEA reference grid), otherwise
#' wrong results may arise.
#' @param fun Character or function. Aggregation function passed to
#'   `terra::extract()` (e.g., `"mean"`, `"median"`, `"sum"`).
#' @param ... Additional arguments passed to `terra::extract()`.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Projects the input `grid` to overlay on the input `layers`.
#'   \item Extracts `layers` values onto the projected grid using `fun`.
#'   \item Builds a SpatRaster with the same dimensions as `grid`.
#'   \item Rasterizes each extracted layer onto this SpatRaster.
#' }
#'
#' @return A SpatRaster with the same number of layers as `layers`,
#'   re-gridded to the resolution and extent of the target grid.
#'
#' @examples
#' \dontrun{
#' eea50 <- terra::vect("https://sdi.eea.europa.eu/datashare/s/jokLrYcEBFiJRyo/download?path=%2FGPKG&files=EEA_50km_grid_v2024.gpkg")
#'
#' ctry <- geodata::gadm(country = "Portugal", level = 0, path = tempdir())
#' ctry_prj <- terra::project(ctry, eea50)
#'
#' plot(ctry)
#' plot(ctry_prj)
#'
#' grid <- terra::subset(eea50, ctry_prj)  # smaller object for faster computing
#' plot(grid, lwd = 0.2, add = TRUE)
#'
#' layers <- geodata::worldclim_global(var = "bio", res = 5, path = tempdir())
#' plot(layers[[1:4]])
#' plot(terra::crop(layers[[1:4]], ctry))
#'
#' out <- regrid(layers = layers, grid = grid, fun = "mean", touches = TRUE, na.rm = TRUE)
#' plot(out[[1:4]])
#'
#' plot(out[[1]])
#' plot(grid, lwd = 0.2, add = TRUE)
#' }
#'
#' @importFrom terra project extract values ext rast rasterize nlyr
#' @export

regrid <- function(layers, grid, fun = "mean", verbosity = 1, ...) {

  if (verbosity > 0) message("projecting 'grid' to 'layers'")
  grid_prj <- terra::project(grid, layers)

  if (verbosity > 0) message("extracting 'layers' to projected 'grid'\n(can take a while for large grids...)")
  extr <- terra::extract(layers, grid_prj, fun = fun, ...)
  terra::values(grid) <- data.frame(terra::values(grid), extr)

  ext1 <- terra::ext(grid[1, ])
  dx <- ext1[2] - ext1[1]
  # dy <- ext1[4] - ext1[3]
  # if (!isTRUE(all.equal(dx, dy))) {
  #   warning("1st 'grid' cell not square! result may be incorrect")
  # }  # seems to be generating false positives?
  template <- terra::rast(grid, resolution = dx)

  out <- terra::rast(template, nlyrs = terra::nlyr(layers))
  names(out) <- names(layers)
  if (verbosity == 1) message("rasterizing layers on projected grid")
  for (l in names(layers)) {
    if (verbosity > 1) message("rasterizing ", l, " on projected grid")
    out[[l]] <- terra::rasterize(grid, template, field = l)
  }

  if (verbosity > 0) message("finished!")
  out
}
