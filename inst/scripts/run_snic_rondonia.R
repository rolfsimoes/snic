#!/usr/bin/env Rscript

# SNIC segmentation demo for the Rondônia Sentinel-2 stack.

if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The 'terra' package must be installed to run this script.", call. = FALSE)
}

library(snic)

# Locate and load input data
data_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)

files <- c(
    "S2_20LMR_B02_20220630.tif",
    "S2_20LMR_B04_20220630.tif",
    "S2_20LMR_B08_20220630.tif",
    "S2_20LMR_B12_20220630.tif"
)

paths <- file.path(data_dir, files)
s2_cube <- terra::rast(paths)


# SNIC configuration and execution

grid_step <- 20L
compactness <- 0.1
seeds <- snic_rect_grid(
    s2_cube,
    spacing = c(grid_step, grid_step),
    padding = c(grid_step %/% 2L, grid_step %/% 2L)
)
seeds <- round(seeds)
storage.mode(seeds) <- "integer"

segments <- snic(
    s2_cube,
    seeds = seeds,
    compactness = compactness
)

# Vectorise segments
segments_poly <- terra::as.polygons(
    segments,
    dissolve = TRUE,
    na.rm = TRUE
)


# Visualisation

terra::plotRGB(
    s2_cube,
    r = 4, # B12
    g = 3, # B08
    b = 1, # B02
    stretch = "lin",
    mar = c(3, 3, 1, 5),
    main = sprintf(
        "SNIC (%d x %d, step=%d, compactness=%.2f)",
        nrow(s2_cube), ncol(s2_cube), grid_step, compactness
    )
)

terra::plot(
    segments_poly,
    add = TRUE,
    border = "#FFFF00",
    col = NA,
    lwd = 0.8
)

# Uncomment below to save the segmentation raster.
# output_path <- file.path(tempdir(), "snic_rondonia_segments_full.tif")
# writeRaster(segments, output_path, overwrite = TRUE, datatype = "INT4U")
# message("Segmentation saved to: ", output_path)
