#' Re-grid raster layer(s) onto an equal-area square-cell grid
#'
#' @description
#' This function extracts the values of a set of raster layers onto an
#' equal-area square-cell vector polygon grid, then rasterizes the values onto
#' a layer with the same extent, resolution and coordinate reference system as
#' the input vector grid. It is useful for converting lon-lat variable layers
#' into equal-area pixel grids (matching e.g. the EEA reference grid).
#'
#' @param layers SpatRaster (or an object that can be coerced to it)
#' containing the input layer(s) to be re-gridded.
#' @param grid SpatVector (or an object that can be coerced to it)
#' defining the target grid. Must consist of square equal-area polygons (e.g.
#' the EEA reference grid), otherwise results will be incorrect.
#' @param fun aggregation/summarizing function (default "mean") passed to
#' `terra::extract()` (or to `exactextractr::exact_extract()` if
#' exactextract = TRUE - see below).
#' @param densif positive integer value (default 0) indicating the number of equal parts in which to divide each line segment of `grid`, in which case `terra::densify` is used to avoid the path changing too much when projecting (if `layers` have a different CRS). This normally makes a difference when grid cells are very large and at polar latitudes.
#' @param exactextract logical (default FALSE) specifying whether the
#' extraction of `layers` values to the polygon `grid` should be performed with
#' the 'exactextractr' package rather than the 'terra' package. Can be faster,
#' but requires package 'exactextractr' (>= 0.10.1) to be installed.
#' @param verbosity integer indicating the amount of progress messages to
#' display. The default is 1, for an intermediate amount of messages. Currently
#' meaningful values are integers from 0 to 2.
#' @param ... Additional arguments passed to `terra::extract()`.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Densifies the input vector `grid` (if densif > 0) by introducing additional vertices
#'   \item Projects the densified vector `grid` to properly overlay the input raster `layers`
#'   \item Extracts/summarizes `layers` values onto the projected `grid`, using
#'   the function specified in `fun`
#'   \item Makes a SpatRaster grid with the same CRS and dimensions (extent, cell size) as the input `grid`
#'   \item Rasterizes each extracted layer onto this SpatRaster grid
#' }
#'
#' @return The function returns a SpatRaster of the input `layers` re-gridded
#' to match the input `grid`.
#'
#' @examples
#' \dontrun{
#' eea_url <- paste0("https://sdi.eea.europa.eu/datashare/s/jokLrYcEBFiJRyo/",
#' "download?path=%2FGPKG&files=EEA_50km_grid_v2024.gpkg")
#'
#' eea50 <- terra::vect(eea_url)
#'
#'
#' ctry <- geodata::gadm(country = "Portugal", level = 0, path = tempdir())
#'
#' ctry_prj <- terra::project(ctry, eea50)
#'
#'
#' terra::plot(ctry)
#'
#' terra::plot(ctry_prj)
#'
#'
#' grid <- terra::subset(eea50, ctry_prj)  # smaller object for quicker example
#' terra::plot(grid, lwd = 0.2, add = TRUE)
#'
#'
#' layers <- geodata::worldclim_global(var = "bio", res = 5, path = tempdir())
#' terra::plot(layers[[1:4]])
#'
#' terra::plot(terra::crop(layers[[1:4]], ctry))
#'
#'
#' out <- regrid(layers = layers, grid = grid, fun = "mean",
#' na.rm = TRUE, touches = TRUE)
#'
#' terra::plot(out[[1:4]])
#'
#'
#' terra::plot(out[[1]])
#'
#' terra::plot(grid, lwd = 0.2, add = TRUE)
#'
#'
#' out2 <- regrid(layers = layers, grid = grid, fun = "mean",
#' exactextract = TRUE)
#'
#'
#' terra::plot(out2[[1]])
#'
#' terra::plot(grid, lwd = 0.2, add = TRUE)
#' }
#'
#' @importFrom terra densify ext extract nlyr project rast rasterize values vect
#' @export

regrid <- function(layers,
                   grid,
                   fun = "mean",
                   densif = 0,
                   exactextract = FALSE,
                   verbosity = 1,
                   ...) {

  if (exactextract && !requireNamespace("exactextractr", quietly = TRUE))
    stop("exactextract=TRUE requires the 'exactextractr' package to be installed")

  if (!inherits(layers, "SpatRaster")) layers <- terra::rast(layers)
  if (!inherits(grid, "SpatVector")) grid <- terra::vect(grid)

  ext1 <- terra::ext(grid[1, ])  # assumes all cells same size as 1st cell
  dx <- unname(ext1[2] - ext1[1])
  dy <- unname(ext1[4] - ext1[3])
  if (!isTRUE(all.equal(dx, dy))) {
    warning("1st 'grid' cell not square! result may be incorrect")
  }

  if (densif > 0) {
    if (verbosity > 0) message("densifying 'grid'")
    grid_dens <- terra::densify(grid, interval = dx / densif, equalize = TRUE)
  }

  if (verbosity > 0) message("projecting 'grid' to overlay 'layers'")
  grid_prj <- terra::project(grid_dens, layers)

  if (verbosity > 0) message("extracting 'layers' to projected 'grid' (can take a while for dense grids...)")
  if (exactextract) {
    extr <- exactextractr::exact_extract(layers, sf::st_as_sf(grid_prj), fun = fun)  # error if additional arguments...
    names(extr) <- gsub(paste0(fun, "."), "", names(extr))
  }
  else extr <- terra::extract(layers, grid_prj, fun = fun, ...)

  terra::values(grid) <- data.frame(terra::values(grid), extr)

  rst <- terra::rast(grid, resolution = dx)

  out <- terra::rast(rst, nlyrs = terra::nlyr(layers))
  names(out) <- names(layers)
  if (verbosity == 1) message("rasterizing input 'grid' with extracted 'layers' values")
  for (l in names(layers)) {
    if (verbosity > 1) message("rasterizing input 'grid' with with extracted ", l)
    out[[l]] <- terra::rasterize(grid, rst, field = l)
  }

  if (verbosity > 0) message("finished!")
  out
}
