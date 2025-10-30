#' Plot SNIC seeds on image data
#'
#' Overlay SNIC seed coordinates on an image represented as a matrix or a
#' [terra::SpatRaster].
#'
#' @param x Image data.
#' @param y Ignored (included for compatibility with [graphics::plot()]).
#' @param ... For matrix inputs, additional arguments include `seeds`, `width`,
#'   `height`, `band`, `col`, `seed_col`, `seed_pch`, `seed_cex`, and any other
#'   parameters passed on to [graphics::image()]. For `SpatRaster` inputs the
#'   same seed-related arguments are recognised; remaining arguments are
#'   forwarded to [terra::plot()] when no seeds are supplied.
#' @return Invisibly, `NULL`.
#' @name plot_snic
NULL

#' @rdname plot_snic
#' @export
#' @method plot array
plot.array <- function(x, y, ...) {
    dots <- list(...)

    if (length(dim(x)) != 3L) {
        stop("`x` must be a 3D array with dimensions (height, width, bands)",
            call. = FALSE
        )
    }

    height <- dim(x)[1L]
    width <- dim(x)[2L]
    nbands <- dim(x)[3L]

    seeds <- dots$seeds
    band <- if (!is.null(dots$band)) {
        as.integer(dots$band)
    } else {
        1L
    }

    if (band < 1L || band > nbands) {
        stop(
            sprintf(
                "Invalid `band` index (%d). Array has %d bands.",
                band, nbands
            ),
            call. = FALSE
        )
    }

    col <- if (!is.null(dots$col)) dots$col else grDevices::grey.colors(64)
    seed_col <- if (!is.null(dots$seed_col)) dots$seed_col else "red"
    seed_pch <- if (!is.null(dots$seed_pch)) dots$seed_pch else 4
    seed_cex <- if (!is.null(dots$seed_cex)) dots$seed_cex else 1

    # remove handled args
    dots[c("seeds", "band", "col", "seed_col", "seed_pch", "seed_cex")] <- NULL

    # call internal plotting function
    plot_snic_matrix(
        img = x[, , band, drop = FALSE],
        seeds = seeds,
        band = band,
        col = col,
        seed_col = seed_col,
        seed_pch = seed_pch,
        seed_cex = seed_cex,
        image_args = dots
    )
}

plot_snic_matrix <- function(img,
                             seeds = NULL,
                             band = 1L,
                             col = grDevices::grey.colors(64),
                             seed_col = "red",
                             seed_pch = 4,
                             seed_cex = 1,
                             image_args = list()) {
    stopifnot(is.array(img), is.numeric(img))

    dims <- dim(img)
    if (length(dims) != 3L) {
        stop("`img` must be a 3D array with dimensions (height, width, bands)",
            call. = FALSE
        )
    }

    height <- dims[1L]
    width <- dims[2L]
    nbands <- dims[3L]

    # Band selection or RGB composite
    if (nbands >= 3L && (is.null(band) || length(band) > 1L)) {
        # If 3 bands and no specific band: use RGB
        band <- 1:3
    } else {
        band <- as.integer(band)
        if (any(band < 1L | band > nbands)) {
            stop(sprintf("Invalid band index (max = %d)", nbands),
                call. = FALSE
            )
        }
    }

    # Prepare display matrix
    if (length(band) == 1L) {
        display_mat <- t(img[, , band])
        color_mode <- "grayscale"
    } else if (length(band) == 3L) {
        # Normalize each channel 0–1 for raster plotting
        r <- t(img[, , band[1L]])
        g <- t(img[, , band[2L]])
        b <- t(img[, , band[3L]])
        minv <- min(c(r, g, b), na.rm = TRUE)
        maxv <- max(c(r, g, b), na.rm = TRUE)
        rgb_array <- array((c(r, g, b) - minv) / (maxv - minv),
            dim = c(width, height, 3L)
        )
        color_mode <- "rgb"
    } else {
        stop("Can only plot one band (grayscale) or three bands (RGB)",
            call. = FALSE
        )
    }

    # Validate seeds
    if (!is.null(seeds)) {
        if (!is.matrix(seeds) || ncol(seeds) != 2L) {
            stop("`seeds` must be a matrix with two columns (row, column)",
                call. = FALSE
            )
        }
        if (!is.integer(seeds) && !all(seeds == as.integer(seeds))) {
            stop("`seeds` must contain integer coordinates", call. = FALSE)
        }
        seeds <- matrix(as.integer(seeds), ncol = 2L)
        if (nrow(seeds) == 0L) {
            stop("`seeds` must contain at least one coordinate", call. = FALSE)
        }
        if (any(seeds[, 1L] < 1L | seeds[, 1L] > height |
            seeds[, 2L] < 1L | seeds[, 2L] > width)) {
            stop("`seeds` must lie within the image bounds", call. = FALSE)
        }
    }

    # Plot image
    if (color_mode == "grayscale") {
        default_args <- list(
            x = 0.5 + 0:width,
            y = 0.5 + 0:height,
            z = display_mat,
            col = col,
            xlab = "j",
            ylab = "i",
            asp = 1,
            useRaster = TRUE,
            ylim = c(height + 0.5, 0.5)
        )
        args <- utils::modifyList(default_args, image_args)
        do.call(graphics::image, args)
    } else {
        graphics::rasterImage(rgb_array, 1, 1, width, height,
            interpolate = FALSE
        )
    }

    # Overlay seeds
    if (!is.null(seeds)) {
        graphics::points(seeds[, 2L], seeds[, 1L],
            pch = seed_pch, col = seed_col, cex = seed_cex
        )
    }

    invisible(NULL)
}
