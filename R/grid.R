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
#' The helper \code{\link{snic_count_seeds}} reports how many seeds would be
#' generated for given spacing and padding, without computing coordinates.
#'
#' If \code{x} has a coordinate reference system, the returned matrix includes
#' additional geographic coordinates (\code{lon}, \code{lat}) in
#' \code{EPSG:4326}.
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
#' \dontrun{
#' if (requireNamespace("terra", quietly = TRUE)) {
#'     tiff_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     files <- file.path(
#'         tiff_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'
#'     s2 <- terra::aggregate(terra::rast(files), fact = 5)
#'
#'     seeds <- snic_grid(
#'         s2,
#'         type = "rectangular",
#'         spacing = 10L,
#'         padding = 20L
#'     )
#'
#'     head(seeds)
#' }
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
    check_grid_args(x, spacing, padding)
    seeds <- switch(type,
        rectangular = {
            grid_rect(x, spacing, padding)
        },
        diamond = {
            grid_diamond(x, spacing, padding)
        },
        hexagonal = {
            grid_hex(x, spacing, padding)
        },
        random = {
            grid_random(x, spacing, padding)
        },
        stop(.msg("grid_type_invalid"), call. = FALSE)
    )

    if (!has_crs(x)) {
        return(seeds)
    }
    rc_to_wgs84(x, seeds)
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
#' @param seeds Optional existing seed set to display and extend. May be given as:
#'   \itemize{
#'     \item a two-column data frame \code{(lat, lon)} in \code{EPSG:4326}, or
#'     \item a two-column data frame \code{(r, c)} containing pixel coordinates.
#'   }
#'   If pixel coordinates are supplied, they are internally converted. If
#'   \code{NULL}, the seed set is initialized empty and populated interactively.
#' @param ... Arguments forwarded to \code{\link{snic_plot}} for display control.
#'   These may include \code{band}, \code{r}, \code{g}, \code{b}, \code{stretch},
#'   \code{seeds_plot_args}, or \code{seg_plot_args}.
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
#' A two-column data frame \code{(lat, lon)} expressing seed positions in
#' \code{EPSG:4326}. The result can be passed directly to \code{\link{snic}}.
#'
#' @seealso
#' \code{\link{snic}}, \code{\link{snic_grid}}, \code{\link{snic_animation}}.
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("terra", quietly = TRUE)) {
#'     tiff_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     files <- file.path(
#'         tiff_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'     s2 <- terra::aggregate(terra::rast(files), fact = 5)
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
    snic_args$seeds <- check_seeds(seeds)

    seeds <- grid_manual(x, snic_args, snic_plot_args)

    if (!has_crs(x)) {
        return(seeds)
    }
    rc_to_wgs84(x, seeds)
}

#' @rdname snic_grid
#' @export
snic_count_seeds <- function(x, spacing, padding = padding / 2) {
    if (length(spacing) == 1L) {
        spacing <- rep(spacing, 2L)
    }
    if (length(padding) == 1L) {
        padding <- rep(padding, 2L)
    }
    check_grid_args(x, spacing, padding)
    prod(round(grid_size(x, spacing, padding)))
}

#' Internal grid utilities (developer documentation)
#'
#' These functions implement the core logic for generating seed coordinates
#' used in SNIC grid-based seeding. They are not exported and should not be
#' called directly by users. All grid functions operate in pixel-index space
#' (row/column). CRS-aware conversion is handled elsewhere.
#'
#' @section Functions:
#' \itemize{
#'
#'   \item \code{check_grid_args(x, spacing, padding)}
#'   Validates input dimensions and parameters. Ensures:
#'   \itemize{
#'     \item \code{x} has positive height and width,
#'     \item \code{spacing} is numeric of length 2 and greater than 1,
#'     \item \code{padding} is numeric of length 2 and non-negative,
#'     \item padding does not eliminate all valid placement area.
#'   }
#'
#'   \item \code{grid_size(x, spacing, padding)}
#'   Computes the number of grid points in each dimension that fit within the
#'   interior region defined by \code{padding}.
#'
#'   \item \code{grid_rect(x, spacing, padding)}
#'   Generates a rectangular grid of seed positions evenly spaced across the
#'   available region.
#'
#'   \item \code{grid_diamond(x, spacing, padding)}
#'   Generates a rectangular grid and a second grid offset diagonally by
#'   half the spacing, producing a diamond pattern. Boundary checks ensure
#'   offset points remain valid.
#'
#'   \item \code{grid_hex(x, spacing, padding)}
#'   Similar to \code{grid_diamond}, but applies axis-dependent spacing to
#'   approximate a hexagonal tiling geometry.
#'
#'   \item \code{grid_random(x, spacing, padding)}
#'   Places \code{prod(grid_size(...))} uniformly sampled seed positions
#'   inside the padded region. Sampling is without replacement.
#'
#'   \item \code{grid_manual(x, snic_args, snic_plot_args)}
#'   Interactive seeding. Displays an image and iteratively updates seeds
#'   based on mouse clicks. Re-runs SNIC and re-plots after each update.
#'   Intended for exploratory inspection, not automated workflows.
#' }
#'
#' @keywords internal
#' @name grid_utils
NULL

#' @rdname grid_utils
check_grid_args <- function(x, spacing, padding) {
    h <- nrow(x)
    w <- ncol(x)
    if (is.null(h) || is.null(w) || h < 1L || w < 1L) {
        stop(.msg("grid_img_min_dimensions"), call. = FALSE)
    }
    if (!is.numeric(spacing)) {
        stop(.msg("grid_spacing_numeric"), call. = FALSE)
    }
    if (length(spacing) != 2L) {
        stop(.msg("grid_spacing_length"), call. = FALSE)
    }
    if (any(!is.finite(spacing))) {
        stop(.msg("grid_spacing_finite"), call. = FALSE)
    }
    if (any(spacing <= 1)) {
        stop(.msg("grid_spacing_greater_than_one"), call. = FALSE)
    }
    if (!is.numeric(padding)) {
        stop(.msg("grid_padding_numeric"), call. = FALSE)
    }
    if (length(padding) != 2L) {
        stop(.msg("grid_padding_length"), call. = FALSE)
    }
    if (any(!is.finite(padding))) {
        stop(.msg("grid_padding_finite"), call. = FALSE)
    }
    if (any(padding < 0)) {
        stop(.msg("grid_padding_non_negative"), call. = FALSE)
    }
    if (padding[[1]] >= h / 2 || padding[[2]] >= w / 2) {
        stop(.msg("grid_padding_no_space"), call. = FALSE)
    }

    invisible(NULL)
}

#' @rdname grid_utils
grid_size <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)
    h <- nrow(x)
    w <- ncol(x)
    n_rows <- floor((h - 2 * padding[[1]] - 1) / spacing[[1]]) + 1L
    n_cols <- floor((w - 2 * padding[[2]] - 1) / spacing[[2]]) + 1L

    as.integer(c(n_rows, n_cols))
}

#' @rdname grid_utils
grid_rect <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    size <- grid_size(x, spacing, padding)
    if (any(!is.finite(size))) {
        stop(.msg("grid_unable_determine_seed_counts"), call. = FALSE)
    }
    if (any(size < 1L)) {
        stop(.msg("grid_no_valid_seed_positions"), call. = FALSE)
    }

    h <- nrow(x)
    w <- ncol(x)
    r0 <- padding[[1]] + 1
    r1 <- h - padding[[1]]
    c0 <- padding[[2]] + 1
    c1 <- w - padding[[2]]

    if (size[[1]] == 1L) {
        rows <- mean(c(r0, r1))
    } else {
        rows <- seq(r0, r1, length.out = size[[1]])
    }
    if (size[[2]] == 1L) {
        cols <- mean(c(c0, c1))
    } else {
        cols <- seq(c0, c1, length.out = size[[2]])
    }

    expand(r = rows, c = cols)
}

#' @rdname grid_utils
grid_diamond <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    h <- nrow(x)
    w <- ncol(x)
    spacing <- spacing * sqrt(2)

    base <- grid_rect(x, spacing, padding)
    shifted <- .seeds(
        r = base$r + spacing[[1]] / 2,
        c = base$c + spacing[[2]] / 2
    )

    keep <- shifted$r <= (h - padding[[1]]) &
        shifted$c <= (w - padding[[2]])
    shifted <- shifted[keep, , drop = FALSE]

    rbind(base, shifted)
}

#' @rdname grid_utils
grid_hex <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    h <- nrow(x)
    w <- ncol(x)
    spacing <- spacing * c(1, sqrt(3))

    base <- grid_rect(x, spacing, padding)
    shifted <- .seeds(
        r = base$r + spacing[[1]] / 2,
        c = base$c + spacing[[2]] / 2
    )
    keep <- shifted$r <= (h - padding[[1]]) &
        shifted$c <= (w - padding[[2]])
    shifted <- shifted[keep, , drop = FALSE]

    rbind(base, shifted)
}

#' @rdname grid_utils
grid_random <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    n <- prod(grid_size(x, spacing, padding))
    if (!is.finite(n)) {
        stop(.msg("grid_unable_determine_seed_counts"), call. = FALSE)
    }
    if (n < 1L) {
        stop(.msg("grid_no_valid_seed_positions"), call. = FALSE)
    }

    h <- nrow(x)
    w <- ncol(x)
    inner_dims <- as.integer(round(c(h, w) - 2 * padding))
    area <- prod(inner_dims)

    samples <- sample.int(area, n, replace = FALSE)
    s <- arrayInd(samples, inner_dims)

    .seeds(r = s[, 1L] + padding[[1]], c = s[, 2L] + padding[[2]])
}

#' @rdname grid_utils
grid_manual <- function(x, snic_args, plot_args) {
    if (!dev.interactive(TRUE)) {
        stop(.msg("manual_grid_interactive_only"), call. = FALSE)
    }

    default_snic_args <- list(seeds = NULL, compactness = 0.5)
    snic_args <- utils::modifyList(default_snic_args, snic_args)

    default_plot_args <- list(
        stretch = "lin",
        seeds_plot_args = list(
            pch = 4, col = "#FFFF00", cex = 1
        ),
        seg_plot_args = list(
            border = "#FFFF00", col = NA, lwd = 0.4
        )
    )
    plot_args <- utils::modifyList(default_plot_args, plot_args)

    message(.msg("manual_grid_instructions"))

    snic_args$seeds <- check_seeds(snic_args$seeds)
    snic_args$seeds <- as_seeds_xy(snic_args$seeds, x)
    if (nrow(snic_args$seeds)) {
        plot_args$seeds <- snic_args$seeds
        plot_args$seg <- do.call(snic, c(list(x), snic_args))
    }
    do.call(snic_plot, c(list(x), plot_args))

    repeat {
        p <- graphics::locator(n = 1)
        if (is.null(p)) break

        new_seed <- .seeds(x = p$x, y = p$y) # in x CRS
        if (any(is.na(xy_to_rc(x, new_seed)))) next # outside image

        snic_args$seeds <- append_seed(snic_args$seeds, new_seed)
        plot_args$seg <- do.call(snic, c(list(x), snic_args))
        plot_args$seeds <- snic_args$seeds
        do.call(snic_plot, c(list(x), plot_args))
    }

    as_seeds_rc(snic_args$seeds, x)
}
