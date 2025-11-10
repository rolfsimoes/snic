## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 6,
  fig.height = 5
)
if (!requireNamespace("terra", quietly = TRUE)) {
  knitr::knit_exit()
}
library(snic)
library(terra)

## ----data---------------------------------------------------------------------
data_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
bands <- file.path(
  data_dir,
  c(
    "S2_20LMR_B02_20220630.tif",
    "S2_20LMR_B04_20220630.tif",
    "S2_20LMR_B08_20220630.tif",
    "S2_20LMR_B12_20220630.tif"
  )
)
s2 <- rast(bands)
s2_demo <- aggregate(s2, fact = 8)

## ----normalize----------------------------------------------------------------
normalize <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(0, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / s
}

## ----benchmark, cache = FALSE-------------------------------------------------
terraOptions(progress = 0)

single_core <- system.time({
  demo_single <- terra::app(s2_demo, normalize, cores = 1)
})

multi_core <- tryCatch({
  system.time({
    demo_multi <- terra::app(s2_demo, normalize, cores = 2)
  })
}, error = function(e) {
  message("Skipping multi-core example: ", conditionMessage(e))
  single_core * NA_real_
})

if (exists("demo_single")) rm(demo_single)
if (exists("demo_multi")) rm(demo_multi)

rbind(
  single_core = single_core,
  multi_core = multi_core
)[, c("user.self", "elapsed")]

## ----segment------------------------------------------------------------------
s2_scaled <- tryCatch(
  app(s2_demo, normalize, cores = 2),
  error = function(e) {
    message("Falling back to single-core scaling: ", conditionMessage(e))
    app(s2_demo, normalize, cores = 1)
  }
)

spacing <- 30L
seeds <- snic_grid_rect(s2_scaled, spacing = spacing)
seg <- snic(s2_scaled, seeds = seeds, compactness = 0.6)

## ----segment-plot, fig.width = 7, fig.height = 4.5----------------------------
snic_plot(
  s2_scaled,
  r = 3, g = 2, b = 1,
  stretch = "lin",
  seg = seg,
  seg_plot_args = list(border = "#FFD700", col = NA, lwd = 0.6)
)

