#' @rdname snic
#' @export
snic.SpatRaster <- function(img,
                            seeds,
                            compactness = 1.0,
                            ...) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop("Package 'terra' must be installed to handle SpatRaster input.",
            call. = FALSE
        )
    }

    if (!inherits(img, "SpatRaster")) {
        stop("argument 'img' must be a 'SpatRaster' object", call. = FALSE)
    }

    # Check and prapare seeds to integer matrix
    seeds <- .prepare_seeds(seeds)

    extra <- list(...)
    if (length(extra)) {
        bad_args <- names(extra)
        bad_args[!nzchar(bad_args)] <-
            paste0("<unnamed ", seq_len(sum(!nzchar(bad_args))), ">")
        stop("Unused arguments for SpatRaster input: ",
            paste(bad_args, collapse = ", "),
            call. = FALSE
        )
    }

    # Get a row-major 1D vector (height * width * bands)
    img_data <- terra::values(img, mat = FALSE)
    if (is.null(img_data)) {
        stop("Unable to extract pixel values from the SpatRaster input.",
            call. = FALSE
        )
    }

    # Set dimensions (height, width, bands) in-place
    dims <- dim(img)
    .set_dim(img_data, dims)

    img_data <- .snic(
        img_data,
        seeds,
        as.numeric(compactness),
        order = "C"
    )

    # Set dimensions (height * width, bands=1L) in-place
    .set_dim(img_data, c(dims[[1]] * dims[[2]], 1L))

    result <- terra::rast(img, nlyrs = 1L, vals = img_data)
    names(result) <- "snic"
    result
}

#' @rdname snic_plot
#' @export
snic_plot.SpatRaster <- function(x,
                                 band = 1L,
                                 r = NULL,
                                 g = NULL,
                                 b = NULL,
                                 col = grDevices::hcl.colors(64L, palette = "Spectral"),
                                 stretch = "lin",
                                 ...,
                                 seeds = NULL,
                                 seeds_plot_args = list(
                                     pch = 4, col = "#FFFF00", cex = 1
                                 ),
                                 seg = NULL,
                                 seg_plot_args = list(
                                     border = "#FFFF00", col = NA, lwd = 0.4
                                 )) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop("package 'terra' must be installed to handle SpatRaster input.",
            call. = FALSE
        )
    }
    if (!inherits(x, "SpatRaster")) {
        stop("argument 'x' must be a 'SpatRaster' object", call. = FALSE)
    }

    nbands <- terra::nlyr(x)
    height <- terra::nrow(x)
    width <- terra::ncol(x)

    rgb_requested <- !is.null(r) || !is.null(g) || !is.null(b)

    if (rgb_requested) {
        if (is.null(r) || is.null(g) || is.null(b)) {
            stop("parameters 'r', 'g', and 'b' must all be supplied together", call. = FALSE)
        }
        rgb_idx <- as.integer(c(r, g, b))
        if (any(is.na(rgb_idx))) {
            stop("parameters 'r', 'g', and 'b' must be integer indices", call. = FALSE)
        }
        if (any(rgb_idx < 1L | rgb_idx > nbands)) {
            stop("invalid RGB band index. Raster has %d bands.", call. = FALSE)
        }
        terra::plotRGB(
            x,
            r = rgb_idx[1L],
            g = rgb_idx[2L],
            b = rgb_idx[3L],
            stretch = stretch, ...
        )
    } else {
        band <- as.integer(band)
        if (length(band) != 1L || is.na(band)) {
            stop("argument 'band' must be a single integer index", call. = FALSE)
        }
        if (band < 1L || band > nbands) {
            stop("argument 'band' has invalid band index.", call. = FALSE)
        }
        terra::plot(x[[band]], col = col, stretch = stretch, ...)
    }

    seeds <- .prepare_plot_seeds(seeds, height, width)

    if (!is.null(seeds)) {
        cells <- terra::cellFromRowCol(x, seeds[, 1L], seeds[, 2L])
        xy <- terra::xyFromCell(x, cells)
        if (is.null(seeds_plot_args)) {
            seeds_plot_args <- list()
        }
        seed_defaults <- list(pch = 4, col = "red", cex = 1)
        seed_args <- utils::modifyList(seed_defaults, seeds_plot_args)
        do.call(
            graphics::points,
            c(
                list(x = xy[, 1L], y = xy[, 2L]),
                seed_args
            )
        )
    }

    if (!is.null(seg)) {
        if (is.null(seg_plot_args)) {
            seg_plot_args <- list()
        }
        seg_plot_args$add <- TRUE

        seg_obj <- seg
        if (inherits(seg, "SpatRaster")) {
            seg_obj <- terra::as.polygons(seg, dissolve = TRUE, na.rm = TRUE)
        } else if (inherits(seg, "SpatVector")) {
            # already suitable
        } else {
            stop("argument 'seg' must be a SpatRaster or SpatVector", call. = FALSE)
        }

        do.call(terra::plot, c(list(seg_obj), seg_plot_args))
    }

    invisible(NULL)
}
