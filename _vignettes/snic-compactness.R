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
band_names <- c(
  "S2_20LMR_B02_20220630.tif",
  "S2_20LMR_B04_20220630.tif",
  "S2_20LMR_B08_20220630.tif",
  "S2_20LMR_B12_20220630.tif"
)
s2 <- rast(file.path(data_dir, band_names))
s2_demo <- aggregate(s2, fact = 8)

## ----seeds--------------------------------------------------------------------
spacing <- 28L
seeds <- snic_grid_rect(s2_demo, spacing = spacing)
length(seeds[, 1])

## ----compactness-run----------------------------------------------------------
compactness_values <- c(0.05, 0.5, 2)
segments <- lapply(
  compactness_values,
  function(cmp) {
    snic(s2_demo, seeds = seeds, compactness = cmp)
  }
)
names(segments) <- paste0("c=", compactness_values)
segments

## ----compactness-plot, fig.width = 10, fig.height = 4-------------------------
oldpar <- par(mfrow = c(1, 3), mar = c(2.5, 2.5, 2, 0.5))
on.exit(par(oldpar), add = TRUE)
for (i in seq_along(segments)) {
  snic_plot(
    s2_demo,
    r = 3, g = 2, b = 1,
    stretch = "lin",
    seg = segments[[i]],
    seg_plot_args = list(border = "#FFD700", col = NA, lwd = 0.6),
    main = names(segments)[[i]]
  )
}

