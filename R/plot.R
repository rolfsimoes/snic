#' Plot SNIC imagery
#'
#' Render image data processed by SNIC either from in-memory numeric arrays
#' or from \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} objects
#' provided by the \pkg{terra} package. The function supports plotting a
#' single band (default grayscale palette) or a three-channel RGB composite,
#' with optional overlays for seed points and segmentation boundaries.
#'
#' @param x Image data. For the array method this must be a numeric array
#'   with dimensions \code{(height, width, bands)}. For the raster method
#'   the object must be a \code{\link[terra:SpatRaster-class]{SpatRaster}}.
#' @param band Integer index of the band to display when producing a
#'   single-band plot. Defaults to the first band.
#' @param r,g,b Integer indices (1-based) of the bands to use when composing
#'   an RGB plot. All three must be supplied to trigger RGB rendering and the
#'   image must contain at least three bands.
#' @param col Color palette used for single-band plots. Ignored for RGB plots.
#' @param stretch Character string indicating the contrast-stretching method.
#'   Determines how band values are scaled to the \eqn{[0, 1]} range before
#'   plotting. One of:
#'   \itemize{
#'     \item \code{"none"}: no scaling; input values are used directly.
#'     \item \code{"lin"}: linear stretch based on the minimum and maximum
#'       values (default).
#'     \item \code{"hist"}: histogram equalization (redistribute values to
#'       equalize the color histogram).
#'     \item \code{"sd"}: standard deviation stretch (clip to
#'       \eqn{\text{mean} \pm 2 \times \text{sd}}, then scale).
#'   }
#'   Non-numeric arrays or bands with only constant values are plotted as-is.
#' @param ... Additional arguments forwarded to the underlying plotting
#'   function. For arrays, these are passed to
#'   \code{\link[graphics:image]{graphics::image()}}; for raster inputs they
#'   are forwarded to \code{\link[terra:plot]{terra::plot()}} (single band)
#'   or \code{\link[terra:plotRGB]{terra::plotRGB()}} (RGB composites).
#' @param seeds Optional object containing seed coordinates with
#'   columns \code{r} and \code{c}. Additional columns are preserved; when
#'   plotting \code{\link[terra:SpatRaster-class]{SpatRaster}} inputs,
#'   \code{lat} and \code{lon} columns expressed in \code{"EPSG:4326"} (when
#'   present) are projected to the raster's CRS and take precedence over the
#'   provided \code{r}/\code{c} indices.
#' @param seeds_plot_args Optional named list with additional arguments passed
#'   to \code{\link[graphics:points]{graphics::points()}} when drawing
#'   \code{seeds}.
#' @param seg For \code{\link[terra:SpatRaster-class]{SpatRaster}} inputs, an
#'   optional segmentation raster (integer labels) or already vectorized
#'   segments (a \code{\link[terra:SpatVector-class]{terra::SpatVector}}) to
#'   be drawn over the image.
#' @param seg_plot_args Named list of arguments forwarded to
#'   \code{\link[terra:plot]{terra::plot()}} for the \code{seg} overlay. The
#'   argument \code{add = TRUE} is set automatically when not supplied.
#'
#' @return Invisibly, \code{NULL}.
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'     tdir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     files <- file.path(
#'         tdir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'
#'     # Load and optionally downsample for faster segmentation
#'     s2 <- terra::aggregate(terra::rast(files), fact = 5)
#'
#'     # Generate seeds (lat/lon coordinates because s2 has a CRS)
#'     seeds <- snic_grid(
#'         s2,
#'         type = "rectangular",
#'         spacing = 10L,
#'         padding = 0L
#'     )
#'
#'     # Run segmentation
#'     seg <- snic(s2, seeds = seeds, compactness = 0.1)
#'
#'     # Visualize
#'     snic_plot(
#'         s2,
#'         r = 4, g = 3, b = 1,
#'         stretch = "lin",
#'         seeds = seeds,
#'         seg = seg
#'     )
#' }
#' @export
#' @name snic_plot
NULL

#' @export
#' @rdname snic_plot
snic_plot <- function(x,
                      band = 1L,
                      r = NULL,
                      g = NULL,
                      b = NULL,
                      col = grDevices::hcl.colors(
                          128L,
                          palette = "Spectral"
                      ),
                      stretch = "lin",
                      maxcell = 100000L,
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
        stop(.msg("terra_required"), call. = FALSE)
    }

    x <- check_x(x)
    seeds <- check_seeds(seeds)
    if (!is.null(seg)) {
        seg <- check_x(seg, "seg")
    }

    # convert to SpatRaster
    if (!inherits(x, "SpatRaster")) {
        x <- arr_to_x(rast_tmpl(x), x_to_arr(x))
    }

    plot_core(
        x,
        band = band,
        r = r,
        g = g,
        b = b,
        col = col,
        stretch = stretch,
        maxcell = maxcell,
        ...
    )

    if (nrow(seeds)) {
        seeds_xy <- as_seeds_xy(seeds, x)
        plot_grid(seeds_xy, x, seeds_plot_args, add = TRUE)
    }

    if (!is.null(seg)) {
        plot_segments(seg, seg_plot_args, add = TRUE)
    }

    invisible(NULL)
}

#' @export
#' @rdname snic_plot
snic_plot_grid <- function(seeds, x, ..., add = FALSE) {
    seeds <- check_seeds(seeds)

    seeds_xy <- as_seeds_xy(seeds, x)
    plot_args <- list(...)
    plot_grid(seeds_xy, x, plot_args, add = add)
}

#' @export
#' @rdname snic_plot
snic_plot_segments <- function(x, ..., add = FALSE, maxcell = 100000L) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(.msg("terra_required"), call. = FALSE)
    }

    if (dim(x)[[3]] != 1L) {
        stop(.msg("plot_segments_single_band"), call. = FALSE)
    }

    plot_args <- list(...)

    # convert to SpatRaster
    if (!inherits(x, "SpatRaster")) {
        x <- arr_to_x(
            terra::rast(x), # template
            x_to_arr(x[, , bands, drop = FALSE])
        )
    }
    plot_segments(x, plot_args, add = add)
}

plot_core <- function(x, band, r, g, b, col, stretch, maxcell, ...) {
    if (!inherits(x, "SpatRaster")) {
        stop(.msg("plot_core_invalid_x"), call. = FALSE)
    }

    x <- check_x(x)

    bands <- band
    if (!is.null(r) || !is.null(g) || !is.null(b)) {
        bands <- c(r, g, b)
    }
    n_bands <- dim(x)[[3L]]
    if (any(bands < 1L | bands > n_bands)) {
        stop(.msg("raster_invalid_index"), call. = FALSE)
    }

    if (length(bands) == 3L) {
        terra::plotRGB(
            x,
            r = r,
            g = g,
            b = b,
            mar = 0,
            smooth = FALSE,
            stretch = stretch,
            axes = FALSE,
            maxcell = maxcell,
            ...
        )
    } else {
        terra::plot(
            x, band,
            col = col,
            mar = 0,
            legend = FALSE,
            axes = FALSE,
            maxcell = maxcell,
            smooth = FALSE,
            stretch = stretch,
            ...
        )
    }

    invisible(NULL)
}

plot_grid <- function(seeds_xy, x, plot_args, add) {
    default_args <- list(pch = 4, col = "black", cex = 1)
    plot_args <- utils::modifyList(default_args, plot_args)

    if (!add) {
        oldpar <- graphics::par(mar = c(0, 0, 0, 0))
        on.exit(graphics::par(oldpar), add = TRUE)
        bbox <- x_bbox(x)
        xlim <- c(bbox[[1L]], bbox[[2L]])
        ylim <- c(bbox[[3L]], bbox[[4L]])
        graphics::plot.new()
        graphics::plot.window(xlim = xlim, ylim = ylim)
    }

    do.call(
        graphics::points,
        c(list(x = seeds_xy$x, y = seeds_xy$y), plot_args)
    )
}

plot_segments <- function(x, plot_args, add) {
    default_args <- list(border = "#FFFF00", col = NA, lwd = 0.4)
    plot_args <- utils::modifyList(default_args, plot_args)
    plot_args$add <- add

    # convert to SpatRaster
    if (!inherits(x, "SpatRaster")) {
        x <- arr_to_x(rast_tmpl(x), x_to_arr(x))
    }

    seg <- polygonize(x)
    do.call(terra::plot, c(list(seg), plot_args))
}
