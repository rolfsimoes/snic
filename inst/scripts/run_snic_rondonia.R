#!/usr/bin/env Rscript

if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra package is required. Install it with install.packages(\"terra\").")
}

library(snic)
library(terra)

# helper to resolve files relative to project root
project_file <- function(...) file.path("inst", "extdata", "Rondonia-20LKP", ...)

band_files <- c(
    "SENTINEL-2_MSI_20LKP_B02_2021-08-10.tif",
    "SENTINEL-2_MSI_20LKP_B11_2021-08-10.tif",
    "SENTINEL-2_MSI_20LKP_B8A_2021-08-10.tif",
    "SENTINEL-2_MSI_20LKP_B02_2021-08-26.tif",
    "SENTINEL-2_MSI_20LKP_B11_2021-08-26.tif",
    "SENTINEL-2_MSI_20LKP_B8A_2021-08-26.tif"
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

if (anyNA(img_matrix)) {
    message("Input data contains NA values; SNIC will skip those pixels during segmentation.")
}

connectivity <- 8L

width <- terra::ncol(s2_cube)
height <- terra::nrow(s2_cube)

message(sprintf(
    "Running SNIC on %d pixels (%dx%d) with %d bands (connectivity = %d)...",
    nrow(img_matrix), width, height, ncol(img_matrix), connectivity
))

k <- 400L
segments <- snic::snic(
    img_matrix,
    width = width,
    height = height,
    k = k,
    connectivity = connectivity
)

seg_raster <- terra::rast(s2_cube[[1]])
terra::values(seg_raster) <- segments
terra::varnames(seg_raster) <- "snic"

output_path <- file.path(
    "inst", "extdata", "Rondonia-20LKP",
    "SENTINEL-2_MSI_20LKP_snic_segments_2021-08-10_2021-08-26.tif"
)

terra::writeRaster(seg_raster, output_path, overwrite = TRUE, datatype = "INT4U")

message("SNIC segmentation written to: ", output_path)
