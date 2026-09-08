
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regridR: re-grid raster layers

Most global-coverage raster layers containing variables used in species
distribution models come in unprojected longitude-latitude coordinates,
with a nominal spatial resolution like “*approximately 1x1
km<sup>2</sup> at the equator*”. One problem is that lon-lat pixels are
not equal-area, and they’re not 1x1-km<sup>2</sup> either (or whatever
their nominal resolution is): as the longitude meridians converge
towards the poles, lon-lat pixels cover progressively smaller areas as
we move away from the Equator, and they are already considerably smaller
than their nominal resolution across temperate regions like Europe.
Their actual sizes can be checked e.g. with the `terra::cellSize()`
function.

If we want our raster variables on a grid of pixels that matches an
actually equal-area grid within a given region (such as the [EEA
reference
grid](https://www.eea.europa.eu/en/datahub/datahubitem-view/3c362237-daa4-45e2-8c16-aaadfb1a003b)
in Europe), we can use the `regrid()` function of package `regridR` to
convert raster layers into such equal-area grid. Below is a worked
example. We start by downloading some climate layers from the [CHELSA
website](https://www.chelsa-climate.org/), using the `downloadif()`
function also included in the `regridR` package. This will download
files if they haven’t already been (completely) downloaded and saved in
the destination folder:

``` r
# LOAD REQUIRED PACKAGES ----

library(regridR)

library(terra)
#> terra 1.9.46


# DOWNLOAD SOME VARIABLES ----

# get CHELSA climate links for a couple variables:
links <- linkbuild(c("bio1", "scd"))

# create a folder for receiving downloads:
dir.create("outputs/variables", recursive = TRUE)
#> Warning in dir.create("outputs/variables", recursive = TRUE):
#> 'outputs/variables' already exists

# allow longer download times:
options(timeout = 6000)

# download variables if not already there:
downloadif(links, destdir = "outputs/variables")
#> 1
#> CHELSA_bio1_1981-2010_V.2.1.tif
#> 2
#> CHELSA_scd_1981-2010_V.2.1.tif

# import variables from downloads folder:
layers <- terra::rast(list.files("outputs/variables", full.names = TRUE))

terra::plot(layers, nc = 1)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" alt="" width="100%" />

Next, we will import a vector polygon map of a 10x10-km<sup>2</sup>
equal-area grid recommended by the European Environment Agency (EEA),
using Belgium as an example:

``` r
# IMPORT EQUAL-AREA VECTOR GRID ----

EEAgrid <- terra::vect(system.file("extdata/eea10_belgium.gpkg", 
                                   package = "regridR"))

terra::plot(EEAgrid)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" alt="" width="100%" />

We can project the vector grid to overlay a part of the climate layers
and confirm that they don’t align (and the pixels are not equal-area, or
square, or 1-km<sup>2</sup>), so simply aggregating the raster pixels
into groups of 10x10 pixels wouldn’t be ideal:

``` r
terra::plot(layers[[1]], maxcell = ncell(layers), ext = c(5, 5.4, 49.5, 49.7))

terra::plot(terra::project(EEAgrid, layers), add = TRUE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

So, we’ll use the `regrid()` function of `regridR` to get the climate
layers on a raster grid whose pixels match the input polygon grid cells:

``` r
# RE-GRID LAYERS ----

layers_regrid <- regridR::regrid(layers = layers, grid = EEAgrid, 
                                 na.rm = TRUE, touches = TRUE)

terra::plot(layers_regrid)
```

By default, `regrid()` will use the `mean()` function to summarize the
values of the pixels falling within each polygon grid cell, and the
`terra::zonal()` function to do this summarizing. However, installing
also the `exactextractr` package and **running `regrid()` with the
argument `exactextract = TRUE`** can make the computation **considerably
faster** for large grids, albeit with a slightly different algorithm:

``` r
# RE-GRID LAYERS FASTER ----

layers_regrid <- regridR::regrid(layers = layers, grid = EEAgrid, 
                                 exactextract = TRUE)
#> projecting 'grid' to overlay 'layers'
#> extracting 'layers' to projected 'grid' (can take a while for dense grids...)
#> rasterizing input 'grid' with extracted 'layers' values
#> finished!

terra::plot(layers_regrid, mar = c(1, 1, 2, 3.7))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="" width="100%" />

We can visually check that the output (re-gridded) `layers`’ pixels
align with the input EEA `grid`:

``` r
terra::plot(layers_regrid[[1]])

terra::plot(EEAgrid, lwd = 0.3, add = TRUE)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="" width="100%" />
