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
seed_helper <- snic_grid_hexagon
plot_segmentation <- function(seg_cube, palette, main) {
  seg_mat <- seg_cube[, , 1]
  image(
    x = seq_len(ncol(seg_mat)),
    y = seq_len(nrow(seg_mat)),
    z = t(seg_mat[rev(seq_len(nrow(seg_mat))), ]),
    useRaster = TRUE,
    col = palette,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main
  )
}

## ----magick-example, fig.width = 8, fig.height = 4.5--------------------------
if (!requireNamespace("magick", quietly = TRUE)) {
  cat("Install `magick` to run this example.\n")
} else {
  library(magick)
  logo <- image_scale(image_read("logo:"), "160")
  logo_raw <- image_data(logo, channels = "rgb")
  logo_dim <- dim(logo_raw)
  logo_arr <- as.integer(logo_raw)
  dim(logo_arr) <- logo_dim
  logo_arr <- aperm(logo_arr, c(3, 2, 1)) / 255

  logo_seeds <- seed_helper(logo_arr, spacing = 18L)
  logo_seg <- snic(logo_arr, seeds = logo_seeds, compactness = 0.6)

  oldpar <- par(mfrow = c(1, 2), mar = c(1.5, 1.5, 2, 0.5))
  on.exit(par(oldpar), add = TRUE)
  plot(as.raster(logo_arr), main = "Original image")
  plot_segmentation(logo_seg, hcl.colors(64, "Teal"), "SNIC segments")
}

## ----png-example, fig.width = 8, fig.height = 4.5-----------------------------
if (!requireNamespace("png", quietly = TRUE)) {
  cat("Install `png` to run this example.\n")
} else {
  library(png)
  png_file <- system.file("img", "Rlogo.png", package = "png", mustWork = TRUE)
  rlogo <- readPNG(png_file)
  if (length(dim(rlogo)) == 2) {
    rlogo <- array(rlogo, dim = c(dim(rlogo), 1))
  }
  if (dim(rlogo)[3] == 4) {
    rlogo <- rlogo[, , 1:3]
  }

  rlogo_seeds <- seed_helper(rlogo, spacing = 14L)
  rlogo_seg <- snic(rlogo, seeds = rlogo_seeds, compactness = 0.7)

  oldpar <- par(mfrow = c(1, 2), mar = c(1.5, 1.5, 2, 0.5))
  on.exit(par(oldpar), add = TRUE)
  plot(as.raster(rlogo), main = "PNG input")
  plot_segmentation(rlogo_seg, hcl.colors(64, "Sunset"), "Segment map")
}

## ----jpeg-example, fig.width = 8, fig.height = 4.5----------------------------
if (!requireNamespace("jpeg", quietly = TRUE)) {
  cat("Install `jpeg` to run this example.\n")
} else {
  library(jpeg)
  photo_file <- system.file(
    "img", "Rlogo.jpg", package = "jpeg", mustWork = TRUE
  )
  photo <- readJPEG(photo_file)
  photo_arr <- array(
    photo, dim = c(dim(photo)[1], dim(photo)[2], dim(photo)[3])
  )
  photo_arr <- pmax(pmin(photo_arr, 1), 0)

  photo_seeds <- seed_helper(photo_arr, spacing = 16L)
  photo_seg <- snic(photo_arr, seeds = photo_seeds, compactness = 0.8)

  oldpar <- par(mfrow = c(1, 2), mar = c(1.5, 1.5, 2, 0.5))
  on.exit(par(oldpar), add = TRUE)
  plot(as.raster(photo_arr), main = "JPEG input")
  plot_segmentation(photo_seg, hcl.colors(64, "Viridis"), "Segment map")
}

