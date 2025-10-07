#!/usr/bin/env Rscript

library(snic)
library(terra)

# helper to resolve files relative to project root
project_file <- function(...) system.file("S2-20LKP", ..., package = "snic")

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
    message(
        "Input data contains NA values; SNIC will skip those pixels ",
        "during segmentation."
    )
}

connectivity <- 4L
k <- 1000L
compactness <- 200L

message(sprintf(
    paste0(
        "Running SNIC on image with %d bands, connectivity = %d, ",
        "k = %d, compactness = %d..."
    ),
    ncol(img_matrix), connectivity, k, compactness
))

segments <- snic::snic(
    img_matrix,
    width = terra::ncol(s2_cube),
    height = terra::nrow(s2_cube),
    k = k,
    connectivity = connectivity,
    compactness = compactness
)

seg_raster <- terra::rast(s2_cube[[1]])
terra::values(seg_raster) <- segments
terra::varnames(seg_raster) <- "snic"

output_path <- file.path(
    "~",
    "SENTINEL-2_MSI_20LKP_snic_segments_2021-08-10_2021-08-26.tif"
)

terra::writeRaster(
    seg_raster,
    output_path,
    overwrite = TRUE,
    datatype = "INT4U"
)

message("SNIC segmentation written to: ", output_path)

plot(seg_raster)
