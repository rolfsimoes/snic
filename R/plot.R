#' Plot SNIC imagery
#'
#' Render image data processed by SNIC either from in-memory numeric arrays or
#' from \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} objects provided by
#' the \pkg{terra} package. The function supports plotting a single band
#' (default grayscale palette) or a three-channel RGB composite, with optional
#' overlays for seed points and segmentation boundaries.
#'
#' @param x Image data. For the array method this must be a numeric array with
#'   dimensions \code{(height, width, bands)}. For the raster method the object
#'   must be a \code{\link[terra:SpatRaster-class]{SpatRaster}}.
#' @param band Integer index of the band to display when producing a single-band
#'   plot. Defaults to the first band.
#' @param r,g,b Integer indices (1-based) of the bands to use when composing an
#'   RGB plot. All three must be supplied to trigger RGB rendering and the image
#'   must contain at least three bands.
#' @param col Colour palette used for single-band plots. Ignored for RGB plots.
#' @param stretch Character string indicating the contrast-stretching method.
#'   Determines how band values are scaled to the \eqn{[0, 1]} range before
#'   plotting. One of:
#'   \itemize{
#'     \item \code{"none"}: no scaling; input values are used directly.
#'     \item \code{"lin"}: linear stretch based on the minimum and maximum
#'       values (default).
#'     \item \code{"hist"}: histogram equalisation (redistribute values to
#'       equalise the colour histogram).
#'     \item \code{"sd"}: standard deviation stretch (clip to
#'       \eqn{\text{mean} \pm 2 \times \text{sd}}, then scale).
#'   }
#'   Non-numeric arrays or bands with only constant values are plotted as-is.
#' @param ... Additional arguments forwarded to the underlying plotting
#'   function. For arrays, these are passed to
#'   \code{\link[graphics:image]{graphics::image()}}; for raster inputs they are
#'   forwarded to \code{\link[terra:plot]{terra::plot()}} (single band) or
#'   \code{\link[terra:plotRGB]{terra::plotRGB()}} (RGB composites).
#' @param seeds Optional matrix-like object containing seed coordinates with
#'   columns \code{r} and \code{c}. Coordinates are specified in row/column order
#'   for arrays, and in image row/column order for rasters (converted to
#'   geographic coordinates automatically).
#' @param seeds_plot_args Optional named list with additional arguments passed to
#'   \code{\link[graphics:points]{graphics::points()}} when drawing
#'   \code{seeds}.
#' @param seg For \code{\link[terra:SpatRaster-class]{SpatRaster}} inputs, an optional
#'   segmentation raster (integer labels) or already polygonised segments (a
#'   \code{\link[terra:SpatVector-class]{terra::SpatVector}}) to be drawn over the
#'   image.
#' @param seg_plot_args Named list of arguments forwarded to
#'   \code{\link[terra:plot]{terra::plot()}} for the \code{seg} overlay. The
#'   argument \code{add = TRUE} is set automatically when not supplied.
#'
#' @return Invisibly, \code{NULL}.
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'     tiff_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     band_files <- file.path(
#'         tiff_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif"
#'         )
#'     )
#'     s2 <- terra::rast(band_files)
#'     s2 <- terra::aggregate(s2, factor = 5)
#'     seeds <- rect_grid(
#'         s2,
#'         spacing = c(10L, 10L),
#'         padding = c(5L, 5L)
#'     )
#'     arr <- terra::as.array(s2)
#'     snic_plot(arr, band = 1L, stretch = "none", seeds = seeds)
#'     snic_plot(arr, r = 1L, g = 2L, b = 3L, stretch = "hist", seeds = seeds)
#'     snic_plot(s2, r = 1L, g = 2L, b = 3L, stretch = "lin", seeds = seeds)
#' }
#' @name snic_plot
NULL

#' @rdname snic_plot
#' @export
snic_plot <- function(x,
                      band = 1L,
                      r = NULL,
                      g = NULL,
                      b = NULL,
                      col = grDevices::hcl.colors(64L, palette = "Spectral"),
                      stretch = "lin",
                      ...,
                      seeds = NULL,
                      seeds_plot_args = list(
                          pch = 4, col = "red", cex = 1
                      )) {
    UseMethod("snic_plot")
}

#' @rdname snic_plot
#' @export
snic_plot.array <- function(x,
                            band = 1L,
                            r = NULL,
                            g = NULL,
                            b = NULL,
                            col = grDevices::hcl.colors(64L, palette = "Spectral"),
                            stretch = "lin",
                            ...,
                            seeds = NULL,
                            seeds_plot_args = list(
                                pch = 4, col = "red", cex = 1
                            )) {
    if (!is.array(x) || length(dim(x)) != 3L) {
        stop("argment 'x' must be a 3D array with dimensions (height, width, bands)",
            call. = FALSE
        )
    }
    if (!is.numeric(x)) {
        stop("argument 'x' must be a numeric array", call. = FALSE)
    }

    dims <- dim(x)
    height <- dims[1L]
    width <- dims[2L]
    nbands <- dims[3L]

    rgb_requested <- !is.null(r) || !is.null(g) || !is.null(b)

    if (rgb_requested) {
        if (is.null(r) || is.null(g) || is.null(b)) {
            stop("Parameters 'r', 'g', and 'b' must all be supplied together", call. = FALSE)
        }
        rgb_idx <- as.integer(c(r, g, b))
        if (any(is.na(rgb_idx))) {
            stop("Parameters 'r', 'g', and 'b' must be integer indices", call. = FALSE)
        }
        if (any(rgb_idx < 1L | rgb_idx > nbands)) {
            stop(
                sprintf(
                    "Invalid RGB band index. Array has %d bands.",
                    nbands
                ),
                call. = FALSE
            )
        }
        .plot_rgb_array(
            x,
            r = rgb_idx[1L],
            g = rgb_idx[2L],
            b = rgb_idx[3L],
            stretch = stretch,
            ...
        )
    } else {
        band <- as.integer(band)
        if (length(band) != 1L || is.na(band)) {
            stop("'band' must be a single integer index", call. = FALSE)
        }
        if (band < 1L || band > nbands) {
            stop(
                sprintf(
                    "Invalid 'band' index (%d). Array has %d bands.",
                    band, nbands
                ),
                call. = FALSE
            )
        }
        .plot_array(x, band, col, ..., stretch = stretch)
    }

    seeds <- .prepare_plot_seeds(seeds, height, width)

    if (!is.null(seeds)) {
        if (is.null(seeds_plot_args)) {
            seeds_plot_args <- list()
        }
        seed_defaults <- list(pch = 4, col = "red", cex = 1)
        seed_args <- utils::modifyList(seed_defaults, seeds_plot_args)
        do.call(
            graphics::points,
            c(
                list(x = seeds[, 2L], y = seeds[, 1L]),
                seed_args
            )
        )
    }

    invisible(NULL)
}

.plot_rgb_array <- function(x, r, g, b, stretch, ...) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(
        mar = rep(0, 4),
        oma = rep(0, 4),
        bg = "black",
        plt = c(0, 1, 0, 1),
        xaxs = "i",
        yaxs = "i"
    )

    dims <- dim(x)
    height <- dims[1L]
    width <- dims[2L]

    r_mat <- (x[, , r, drop = TRUE])
    g_mat <- (x[, , g, drop = TRUE])
    b_mat <- (x[, , b, drop = TRUE])

    rgb_array <- array(0, dim = c(height, width, 4L))
    rgb_array[, , 1L] <- .stretch_band(r_mat, stretch)
    rgb_array[, , 2L] <- .stretch_band(g_mat, stretch)
    rgb_array[, , 3L] <- .stretch_band(b_mat, stretch)
    rgb_array[, , 4L] <- 1
    rgb_array[is.nan(rgb_array[, , ])] <- 0

    graphics::plot.new()

    graphics::plot.window(
        xlim = c(0, width),
        ylim = c(0, height),
        asp = 1
    )

    graphics::rasterImage(
        rgb_array,
        xleft = 0,
        ybottom = 0,
        xright = width,
        ytop = height,
        interpolate = FALSE,
        ...
    )
}

.plot_array <- function(x, band, col, stretch, ...) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(
        mar = rep(0, 4),
        oma = rep(0, 4),
        bg = "black",
        plt = c(0, 1, 0, 1),
        xaxs = "i",
        yaxs = "i"
    )

    x <- x[, , band, drop = TRUE]

    dims <- dim(x)
    height <- dims[1L]
    width <- dims[2L]

    display_mat <- .stretch_band(t(x), stretch)

    graphics::image(
        x = 0:width,
        y = 0:height,
        z = display_mat,
        col = col,
        asp = 1,
        useRaster = TRUE,
        ylim = c(height, 0),
        ...
    )
}

.stretch_band <- function(x, method) {
    if (method == "none") {
        rng <- range(x, na.rm = TRUE)
        return((x - rng[1]) / diff(rng))
    }
    if (method == "lin") {
        q <- quantile(x, probs = c(0.02, 0.98), na.rm = TRUE)
        x[x < q[1]] <- q[1]
        x[x > q[2]] <- q[2]
        return((x - q[1]) / diff(q))
    }
    if (method == "sd") {
        m <- mean(x, na.rm = TRUE)
        s <- sd(x, na.rm = TRUE)
        rng <- c(m - 2 * s, m + 2 * s)
        x <- pmin(pmax(x, rng[1]), rng[2])
        return((x - rng[1]) / diff(rng))
    }
    if (method == "hist") {
        p <- stats::ecdf(x[is.finite(x)])
        res <- p(x)
        dim(res) <- dim(x)
        return(res)
    }
    stop("argument 'stretch' has an invalid method", call. = FALSE)
}
