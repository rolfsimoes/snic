#' Spatial grid seeding for SNIC segmentation
#'
#' Generate seed locations on an image following one of four spatial
#' arrangements used in SNIC (Simple Non-Iterative Clustering) segmentation:
#' rectangular, diamond, hexagonal, or random. Works for both numeric arrays
#' and \code{\link[terra:SpatRaster-class]{SpatRaster}} objects.
#'
#' @name snic_grid
#'
#' @param x Image data. For arrays, this must be numeric with dimensions
#'   \code{(height, width, bands)}. For \code{SpatRaster} objects, raster
#'   dimensions are inferred automatically.
#' @param type Character string indicating the spatial pattern to generate.
#'   One of \code{"rectangular"}, \code{"diamond"}, \code{"hexagonal"},
#'   or \code{"random"}.
#' @param spacing Numeric or integer. Either one value (applied to both axes)
#'   or two values \code{(vertical, horizontal)} giving the spacing between
#'   seeds in pixels.
#' @param padding Numeric or integer. Distance from image borders within which
#'   no seeds are placed. May be of length 1 or 2. Defaults to
#'   \code{spacing / 2}.
#' @param ... Currently unused; reserved for future extensions.
#'
#' @details
#' The \code{spacing} parameter controls seed density. Padding shifts the
#' seed grid inward so that seeds are not placed directly on image borders.
#'
#' The spatial arrangements are:
#' \itemize{
#'   \item \code{rectangular}: regular grid aligned with rows and columns.
#'   \item \code{diamond}: alternating row offsets, forming a diamond layout.
#'   \item \code{hexagonal}: alternating offsets approximating a hexagonal
#'     tiling.
#'   \item \code{random}: uniform random placement with similar expected
#'     density.
#' }
#'
#' The helper \code{\link{snic_count_seeds}} estimates how many seeds would be
#' generated for a rectangular lattice with the given spacing and padding,
#' without computing coordinates. For \code{type} = \code{"diamond"} or
#' \code{"hexagonal"}, the actual number of seeds will be up to roughly
#' twice this estimate (minus boundary effects). For \code{"random"}, the
#' estimate corresponds to the expected density.
#'
#' If \code{x} has a coordinate reference system, the returned data frame
#' includes geographic coordinates (\code{lat}, \code{lon}) in \code{EPSG:4326}.
#'
#' @return
#' A data frame containing:
#' \itemize{
#'   \item \code{r}, \code{c} when \code{x} has no CRS.
#'   \item \code{lat}, \code{lon} when \code{x} has a CRS, expressed in
#'     \code{EPSG:4326}.
#' }
#'
#' @seealso
#' \code{\link{snic_count_seeds}} for estimating seed counts.
#'
#' @examples
#' # Example 1: Geospatial raster
#' if (requireNamespace("terra", quietly = TRUE) && terra_is_working()) {
#'     # Load example multi-band image (Sentinel-2 subset) and downsample
#'     tiff_dir <- system.file("demo-geotiff",
#'         package = "snic",
#'         mustWork = TRUE
#'     )
#'     files <- file.path(tiff_dir, c(
#'         "S2_20LMR_B02_20220630.tif",
#'         "S2_20LMR_B04_20220630.tif",
#'         "S2_20LMR_B08_20220630.tif",
#'         "S2_20LMR_B12_20220630.tif"
#'     ))
#'     s2 <- terra::aggregate(terra::rast(files), fact = 8)
#'
#'     # Compare grid types visually using snic_plot for immediate feedback
#'     types <- c("rectangular", "diamond", "hexagonal", "random")
#'     op <- par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
#'     for (tp in types) {
#'         seeds <- snic_grid(s2, type = tp, spacing = 12L, padding = 18L)
#'         snic_plot(
#'             s2,
#'             r = 4, g = 3, b = 1, stretch = "lin",
#'             seeds = seeds,
#'             main = paste("Grid:", tp)
#'         )
#'     }
#'     par(mfrow = c(1, 1))
#'
#'     # Estimate seed counts for planning
#'     snic_count_seeds(s2, spacing = 12L, padding = 18L)
#'     par(op)
#' }
#'
#' # Example 2: In-memory image (JPEG)
#' if (requireNamespace("jpeg", quietly = TRUE)) {
#'     img_path <- system.file(
#'         "demo-jpeg/clownfish.jpeg",
#'         package = "snic",
#'         mustWork = TRUE
#'     )
#'     rgb <- jpeg::readJPEG(img_path)
#'
#'     # Compare grid types visually using snic_plot for immediate feedback
#'     types <- c("rectangular", "diamond", "hexagonal", "random")
#'     op <- par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
#'     for (tp in types) {
#'         seeds <- snic_grid(rgb, type = tp, spacing = 12L, padding = 18L)
#'         snic_plot(
#'             rgb,
#'             r = 1, g = 2, b = 3,
#'             seeds = seeds,
#'             main = paste("Grid:", tp)
#'         )
#'     }
#'     par(mfrow = c(1, 1))
#'     par(op)
#' }
#' @export
snic_grid <- function(x,
                      type = c(
                          "rectangular", "diamond",
                          "hexagonal", "random"
                      ),
                      spacing,
                      padding = spacing / 2,
                      ...) {
    type <- match.arg(type)
    if (length(spacing) == 1L) {
        spacing <- rep(spacing, 2L)
    }
    if (length(padding) == 1L) {
        padding <- rep(padding, 2L)
    }
    .check_grid_args(x, spacing, padding)
    seeds <- switch(type,
        rectangular = .grid_rect(x, spacing, padding),
        diamond = .grid_diamond(x, spacing, padding),
        hexagonal = .grid_hex(x, spacing, padding),
        random = .grid_random(x, spacing, padding),
        stop(.msg("grid_type_invalid"), call. = FALSE)
    )

    if (!.has_crs(x)) {
        return(seeds)
    }
    if (!terra_is_working()) {
        stop(.msg("terra_projection_unavailable"), call. = FALSE)
    }
    .rc_to_wgs84(x, seeds)
}

#' Interactive seed selection for SNIC segmentation
#'
#' Collect seed points interactively by clicking on the image. Each left-click
#' adds a new seed; pressing \code{ESC} ends the session. After each click,
#' SNIC segmentation is recomputed and plotted for visual feedback. This is
#' intended for exploratory and fine-tuning workflows, where automatic seeding
#' may not be ideal.
#'
#' @param x A \code{\link[terra:SpatRaster-class]{SpatRaster}} object
#'   with a valid spatial reference and extent. Mouse clicks are interpreted
#'   in map coordinates.
#' @param seeds Optional existing seed set to display and extend.
#'   If pixel coordinates are supplied, they are internally converted. If
#'   \code{NULL}, the seed set is initialized empty and populated
#'   interactively.
#' @param ... Arguments forwarded to \code{\link{snic_plot}} for display
#'   control. These may include \code{band}, \code{r}, \code{g}, \code{b},
#'   \code{stretch}, \code{seeds_plot_args}, or \code{seg_plot_args}.
#' @param snic_args A list of arguments passed to \code{\link{snic}}, such as
#'   \code{compactness}.
#' @param snic_plot_args A list of display modifiers forwarded to
#'   \code{\link{snic_plot}} when rendering the preview.
#'
#' @details
#' After each new seed is placed interactively, segmentation is recomputed to
#' provide immediate feedback on how the seed placement affects clustering.
#'
#' @return
#' A two-column data frame of seed coordinates. If \code{x} lacks a CRS the
#' result is always pixel indices \code{(r, c)}. When \code{x} has a CRS:
#' \itemize{
#'   \item If \code{seeds} were supplied, their coordinate system is preserved
#'     in the output.
#'   \item Otherwise the result is expressed as \code{(lat, lon)} in
#'     \code{EPSG:4326}.
#' }
#' The output can be passed directly to \code{\link{snic}}.
#'
#' @seealso
#' \code{\link{snic}}, \code{\link{snic_grid}}, \code{\link{snic_animation}}.
#'
#' @examples
#' if (interactive() && requireNamespace("terra", quietly = TRUE) && terra_is_working()) {
#'     tiff_dir <- system.file("demo-geotiff",
#'         package = "snic",
#'         mustWork = TRUE
#'     )
#'     files <- file.path(
#'         tiff_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'     s2 <- terra::aggregate(terra::rast(files), fact = 8)
#'
#'     seeds <- snic_grid_manual(
#'         s2,
#'         snic_args = list(compactness = 0.1),
#'         snic_plot_args = list(r = 4, g = 3, b = 1)
#'     )
#'
#'     seg <- snic(s2, seeds, compactness = 0.1)
#'
#'     snic_plot(
#'         s2,
#'         r = 4, g = 3, b = 1,
#'         stretch = "lin",
#'         seeds = seeds,
#'         seg = seg
#'     )
#' }
#' @export
snic_grid_manual <- function(x,
                             seeds = NULL,
                             ...,
                             snic_args = list(
                                 compactness = 0.5
                             ),
                             snic_plot_args = list(
                                 stretch = "lin",
                                 seeds_plot_args = list(
                                     pch = 4, col = "#FFFF00", cex = 1
                                 ),
                                 seg_plot_args = list(
                                     border = "#FFFF00", col = NA, lwd = 0.4
                                 )
                             )) {
    snic_args$seeds <- .seeds_check(seeds)
    return_type <- if (!is.null(seeds)) .seeds_type(snic_args$seeds) else NULL

    seeds <- .grid_manual(x, snic_args, snic_plot_args)

    if (!.has_crs(x)) {
        return(seeds)
    }
    if (!terra_is_working()) {
        stop(.msg("terra_projection_unavailable"), call. = FALSE)
    }
    if (is.null(return_type)) {
        return(.rc_to_wgs84(x, seeds))
    }

    switch(return_type,
        rc = seeds,
        xy = .rc_to_xy(x, seeds),
        wgs84 = .rc_to_wgs84(x, seeds)
    )
}

#' @rdname snic_grid
#' @export
snic_count_seeds <- function(x, spacing, padding = spacing / 2) {
    if (length(spacing) == 1L) {
        spacing <- rep(spacing, 2L)
    }
    if (length(padding) == 1L) {
        padding <- rep(padding, 2L)
    }
    .check_grid_args(x, spacing, padding)
    prod(round(.grid_count_seeds(x, spacing, padding)))
}
