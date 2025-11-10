## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 6,
  fig.height = 5
)
library(snic)

## ----load-imagery-------------------------------------------------------------
if (requireNamespace("terra", quietly = TRUE)) {
  library(terra)

  data_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
  band_names <- c("S2_20LMR_B02_20220630.tif",
                  "S2_20LMR_B04_20220630.tif",
                  "S2_20LMR_B08_20220630.tif",
                  "S2_20LMR_B12_20220630.tif")
  s2 <- rast(file.path(data_dir, band_names))

  # Aggregate raster for lightweight examples
  s2_small <- aggregate(s2, fact = 6)
  s2_small
} else {
  stop("Package 'terra' is required for this example.")
}

## ----rect-grid----------------------------------------------------------------
spacing_demo <- 30L
seeds_default <- snic_grid_rect(s2_small, spacing = spacing_demo)
head(seeds_default)

## ----count-seeds--------------------------------------------------------------
seed_total <- snic_count_seeds(
  s2_small,
  spacing = spacing_demo,
  padding = spacing_demo / 2
)
seed_total

## ----rect-grid-padding--------------------------------------------------------
seeds_asymmetric <- snic_grid_rect(
  s2_small,
  spacing = c(30, 20),
  padding = c(10, 5)
)
nrow(seeds_asymmetric)

## ----run-snic-----------------------------------------------------------------
comp <- 0.25
seg_rect <- snic(
  s2_small,
  seeds = seeds_default,
  compactness = comp
)
seg_rect

## ----plot-snic, fig.height=4.5, fig.width=7-----------------------------------
snic_plot(
  s2_small,
  r = 3, g = 2, b = 1,
  stretch = "lin",
  seeds = seeds_default,
  seg = seg_rect,
  seg_plot_args = list(border = "#FFFF00", col = NA, lwd = 0.6),
  seeds_plot_args = list(pch = 3, col = "#00FFFF", cex = 0.7)
)

