#!/usr/bin/env Rscript

library(snic)

if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra package must be installed to run this script", call. = FALSE)
}

# helper to resolve files relative to project root
project_file <- function(...) system.file("S2-20LKP", ..., package = "snic")

band_files <- c(
    "S2_MSI_20LKP_B02_2021-08-10.tif",
    "S2_MSI_20LKP_B11_2021-08-10.tif",
    "S2_MSI_20LKP_B8A_2021-08-10.tif",
    "S2_MSI_20LKP_B02_2021-08-26.tif",
    "S2_MSI_20LKP_B11_2021-08-26.tif",
    "S2_MSI_20LKP_B8A_2021-08-26.tif"
)

paths <- vapply(band_files, project_file, character(1))
if (!all(file.exists(paths))) {
    missing <- band_files[!file.exists(paths)]
    stop(sprintf("Missing input rasters: %s", paste(missing, collapse = ", ")))
}

# load all six bands as a single multi-layer raster
s2_cube <- terra::rast(paths)

# snic expects one row per pixel and one column per band
img_matrix <- terra::values(s2_cube, mat = TRUE)

img_matrix_lab <- rgb2lab(
    img_matrix
)

grid_step <- 10L

if (anyNA(img_matrix)) {
    message(
        "Input data contains NA values; SNIC will skip those pixels ",
        "during segmentation."
    )
}

# test grid seeds and visualize
seeds <- snic::snic_seeds_grid(
    img_matrix_lab,
    width = terra::ncol(s2_cube),
    height = terra::nrow(s2_cube),
    step = grid_step
)


plot(img_matrix_lab,
    band = 1,
    width = terra::ncol(s2_cube),
    height = terra::nrow(s2_cube),
    seeds = seeds
)

# run SNIC

compactness <- 10L

message(sprintf(
    paste0(
        "Running SNIC on image with %d bands, ",
        "grid_step = %d, compactness = %d..."
    ),
    ncol(img_matrix), grid_step, compactness
))

# run SNIC with grid seeds (default)
segments <- snic::snic(
    img_matrix_lab,
    width = terra::ncol(s2_cube),
    height = terra::nrow(s2_cube),
    seeds = seeds,
    compactness = compactness,
    grid_step = grid_step
)

plot(segments,
    width = terra::ncol(s2_cube),
    height = terra::nrow(s2_cube)
)


# save segmentation

seg_raster <- terra::rast(s2_cube[[1]])
terra::values(seg_raster) <- as.vector(t(segments))
terra::varnames(seg_raster) <- "snic"

output_path <- file.path(
    "~",
    "S2_MSI_20LKP_snic_segments_2021-08-10_2021-08-10.tif"
)

terra::writeRaster(
    seg_raster,
    output_path,
    overwrite = TRUE,
    datatype = "INT4U"
)

message("SNIC segmentation written to: ", output_path)
