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
#'   \code{seeds}. Defaults to \code{getOption("snic.seeds_plot")}, falling
#'   back to \code{list(pch = 16, col = "#00FFFF", cex = 1)} which mirrors
#'   the internal \code{.plot_seeds()} defaults.
#' @param seg For \code{\link[terra:SpatRaster-class]{SpatRaster}} inputs, an
#'   optional segmentation raster (integer labels) or already vectorized
#'   segments (a \code{\link[terra:SpatVector-class]{terra::SpatVector}}) to
#'   be drawn over the image.
#' @param seg_plot_args Named list of arguments forwarded to
#'   \code{\link[terra:plot]{terra::plot()}} for the \code{seg} overlay. The
#'   argument \code{add = TRUE} is set automatically when not supplied.
#'   Defaults to \code{getOption("snic.seg_plot")}, falling back to
#'   \code{list(border = "#FFD700", col = NA, lwd = 0.6)} which matches the
#'   defaults used inside \code{.plot_segments()}.
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
                      col = getOption(
                          "snic.col",
                          grDevices::hcl.colors(128L, "Spectral")
                      ),
                      stretch = "lin",
                      ...,
                      seeds = NULL,
                      seeds_plot_args = getOption(
                          "snic.seeds_plot",
                          list(pch = 16, col = "#00FFFF", cex = 1)
                      ),
                      seg = NULL,
                      seg_plot_args = getOption(
                          "snic.seg_plot",
                          list(border = "#FFD700", col = NA, lwd = 0.6)
                      )) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(.msg("terra_required"), call. = FALSE)
    }

    x <- check_x(x)
    seeds <- .seeds_check(seeds)
    if (!is.null(seg)) {
        seg <- check_x(seg, "seg")
    }

    # convert to SpatRaster
    if (!inherits(x, "SpatRaster")) {
        x <- arr_to_x(.rast_tmpl(x), x_to_arr(x))
    }

    plot_args <- c(
        list(
            band = get_idx(x, band),
            r = get_idx(x, r),
            g = get_idx(x, g),
            b = get_idx(x, b),
            col = col,
            stretch = stretch
        ),
        list(...)
    )

    .plot_core(x, plot_args)

    if (nrow(seeds)) {
        seeds_xy <- as_seeds_xy(seeds, x)
        .plot_seeds(seeds_xy, x, seeds_plot_args, add = TRUE)
    }

    if (!is.null(seg)) {
        .plot_segments(seg, seg_plot_args, add = TRUE)
    }

    invisible(NULL)
}

#' @export
#' @rdname snic_plot
snic_plot_grid <- function(seeds, x, ..., add = FALSE) {
    seeds <- .seeds_check(seeds)

    seeds_xy <- as_seeds_xy(seeds, x)
    plot_args <- list(...)
    .plot_seeds(seeds_xy, x, plot_args, add = add)
}

#' @export
#' @rdname snic_plot
#' @param add Logical; when \code{TRUE}, seed grids or segment boundaries are
#'   added to the current plot instead of opening a new frame.
snic_plot_segments <- function(x, ..., add = FALSE) {
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
            x_to_arr(x)
        )
    }
    .plot_segments(x, plot_args, add = add)
}
