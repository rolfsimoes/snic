#' Spatial grid seeding for SNIC segmentation
#'
#' Generate seed points following different spatial patterns used in SNIC
#' (Simple Non-Iterative Clustering) segmentation. Four sampling strategies
#' are available: rectangular, diamond, hexagonal, and random.
#'
#' @name snic_grid
#'
#' @param img Image data. For the \code{array} method this should be numeric or
#'   integer with dimensions \code{(height, width, bands)}. For the
#'   \code{\link[terra:SpatRaster-class]{SpatRaster}} method (from \pkg{terra}),
#'   raster dimensions are inferred automatically.
#' @param spacing Numeric or integer vector of length 1 or 2 giving the expected
#'   spacing (in pixels) between neighbouring seeds along the vertical and
#'   horizontal axes. A single value applies equally in both directions.
#' @param padding Numeric or integer vector of length 1 or 2 giving the distance
#'   (in pixels) from the image borders within which no seeds are placed.
#'   Defaults to \code{spacing / 2}, which centres the grid.
#'
#' @details
#' The \code{spacing} parameter directly determines the seed density.
#' The helper \code{\link{count_seeds}} returns the number of seeds that will be
#' generated for a given image size, spacing, and padding.
#'
#' The functions differ in the geometric arrangement of seed centres:
#' \itemize{
#'   \item \code{rect_grid()}: regular rectangular grid.
#'   \item \code{diamond_grid()}: two offset rectangular grids forming a
#'     diamond pattern.
#'   \item \code{hexagonal_grid()}: two offset rectangular grids approximating a
#'     hexagonal (honeycomb) tiling with density correction.
#'   \item \code{random_grid()}: uniformly random seed positions with the same
#'     expected density as the rectangular grid.
#' }
#'
#' The companion function \code{\link{count_seeds}} does not generate
#' coordinates. Instead, it calculates how many seed points would be placed in a
#' given image using the same \code{spacing} and \code{padding} parameters.
#' This is useful for estimating seed density or selecting a spacing value that
#' yields a desired number of segments.
#'
#' In addition to these programmatic grid generators, seeds can also be defined
#' interactively using \code{\link{manual_grid}}, which allows the user to
#' click on the image to place seeds manually. This is useful for inspection,
#' debugging, or small-scale segmentation experiments.
#'
#' @return
#' For \code{rect_grid()}, \code{diamond_grid()}, \code{hexagonal_grid()}, and
#' \code{random_grid()}, the return value is a two-column integer matrix giving
#' the 1-based pixel coordinates \code{(row, column)} of the generated seeds.
#'
#' For \code{count_seeds()}, the return value is a single integer giving the
#' total number of seeds that would be generated for the same parameters.
#'
#' @seealso
#' \code{\link{count_seeds}} for estimating seed counts and
#' \code{\link{manual_grid}} for interactive seed placement.
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'     # Sentinel-2 subset provided with the package
#'     tiff_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     band_files <- file.path(
#'         tiff_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'
#'     # Load and downsample
#'     s2 <- terra::aggregate(terra::rast(band_files), fact = 5)
#'
#'     # Generate seed patterns
#'     set.seed(42)
#'     seeds_rect <- rect_grid(s2, spacing = 10L, padding = 20L)
#'     head(seeds_rect)
#' }
#' @export
rect_grid <- function(img, spacing, padding = NULL) {
    params <- .prepare_grid_args(img, spacing, padding, "rect_grid")
    h <- params$h
    w <- params$w
    spacing <- params$spacing
    padding <- params$padding

    counts <- .count_seeds(h, w, spacing, padding)
    if (any(!is.finite(counts))) {
        stop("Unable to determine seed counts for rect_grid().", call. = FALSE)
    }

    row_start <- padding[[1]] + 1
    row_end <- h - padding[[1]]
    col_start <- padding[[2]] + 1
    col_end <- w - padding[[2]]

    rows <- if (counts[[1]] == 1L) {
        mean(c(row_start, row_end))
    } else {
        seq(row_start, row_end, length.out = counts[[1]])
    }
    cols <- if (counts[[2]] == 1L) {
        mean(c(col_start, col_end))
    } else {
        seq(col_start, col_end, length.out = counts[[2]])
    }

    as.matrix(expand.grid(r = rows, c = cols))
}

#' @rdname snic_grid
#' @export
diamond_grid <- function(img, spacing, padding = NULL) {
    params <- .prepare_grid_args(img, spacing, padding, "diamond_grid")
    spacing <- params$spacing
    padding <- params$padding
    h <- params$h
    w <- params$w

    spacing <- sqrt(sum(spacing * spacing))
    spacing <- c(spacing, spacing)

    # Base grid
    g1 <- rect_grid(img, spacing, padding)

    g2 <- sweep(g1, 2L, spacing / 2, "+")
    g2 <- g2[
        g2[, 1] <= h - padding[[1]] &
            g2[, 2] <= w - padding[[2]], ,
        drop = FALSE
    ]
    rbind(g1, g2)
}

#' @rdname snic_grid
#' @export
hexagonal_grid <- function(img, spacing, padding = NULL) {
    params <- .prepare_grid_args(img, spacing, padding, "hexagonal_grid")
    h <- params$h
    w <- params$w
    spacing <- params$spacing
    padding <- params$padding

    # Adjust spacing for equal density with square grid
    spacing <- spacing * c(1, sqrt(3)) * 1.075 # density correction

    # Base grid
    g1 <- rect_grid(img, spacing, padding)

    g2 <- sweep(g1, 2L, spacing / 2, "+")
    g2 <- g2[
        g2[, 1] <= h - padding[[1]] &
            g2[, 2] <= w - padding[[2]], ,
        drop = FALSE
    ]
    rbind(g1, g2)
}

#' @rdname snic_grid
#' @export
random_grid <- function(img, spacing, padding = NULL) {
    params <- .prepare_grid_args(img, spacing, padding, "random_grid")
    h <- params$h
    w <- params$w
    spacing <- params$spacing
    padding <- params$padding

    # Estimate number of seeds
    counts <- .count_seeds(h, w, spacing, padding)
    if (any(!is.finite(counts))) {
        stop("Unable to determine seed counts for random_grid().", call. = FALSE)
    }
    if (any(counts < 1)) {
        stop(
            "Spacing/padding combination in random_grid() yields no valid seed positions.",
            call. = FALSE
        )
    }
    counts <- as.integer(round(counts))
    n <- prod(counts)

    inner_dims <- c(h, w) - 2 * padding
    if (any(inner_dims <= 0)) {
        stop(
            "Padding in random_grid() leaves no interior area for sampling.",
            call. = FALSE
        )
    }
    inner_dims <- as.integer(round(inner_dims))
    area <- prod(inner_dims)
    if (n > area) {
        stop(
            "Requested seed count exceeds available area in random_grid().",
            call. = FALSE
        )
    }

    samples <- sample.int(area, n, replace = FALSE)
    s <- arrayInd(samples, inner_dims)

    # Valid area (excluding borders)
    sweep(s, 2L, padding, "+")
}

#' Interactive seed selection for SNIC segmentation
#'
#' Collect seed points interactively by clicking on the image. Each left-click
#' adds a new seed; pressing ESC or right-clicking ends the session.
#' After each click, the SNIC segmentation is recomputed and plotted for visual
#' feedback. The function is intended for exploratory or fine-tuning workflows,
#' where automatic seeding may not be ideal.
#'
#' @param img A \code{\link[terra:SpatRaster-class]{SpatRaster}} object (from \pkg{terra})
#'   representing the image to segment. The raster must have valid spatial
#'   referencing and extent, as clicks are interpreted in map coordinates.
#' @param seeds Optional object specifying existing seed coordinates to display
#'   or extend interactively. The expected structure is a two-column matrix
#'   (or any object that can be coerced to such) with columns named \code{r}
#'   (row) and \code{c} (column), giving the 1-based pixel coordinates of seed
#'   locations. If \code{NULL}, an empty seed set is initialised and new seeds
#'   are created through mouse clicks. The returned object follows the same
#'   format and can be passed directly to \code{\link{snic}} for segmentation.
#' @param compactness Numeric scalar controlling the SNIC compactness parameter.
#'   Larger values encourage spatial compactness. Defaults to 1.0.
#' @param ... A list of arguments forwarded to
#'   \code{\link{snic_plot()}} when rendering the preview image. Override
#'   entries such as \code{band}, \code{r}, \code{g}, \code{b}, \code{col},
#'   \code{stretch}, \code{seeds_plot_args}, or \code{seg_plot_args} to tweak
#'   the display.
#'
#' @details
#' The function provides an interactive way to define seeds manually for
#' \code{\link{snic}}. The image is resegmented after each added seed to
#' visualise the progressive effect of new cluster centres.
#'
#' @return
#' A two-column matrix with seed coordinates \code{(r, c)}. The storage mode is
#' not enforced and may remain numeric or double. The return value is suitable
#' for direct use in \code{\link{snic}}.
#'
#' @seealso
#' \code{\link{snic}}, \code{\link{snic_grid}}, \code{\link{rect_grid}},
#' \code{\link{diamond_grid}}, \code{\link{hexagonal_grid}},
#' \code{\link{random_grid}}, \code{\link{snic_plot}}.
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("terra", quietly = TRUE)) {
#'     # Sentinel-2 subset provided with the package
#'     tiff_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     band_files <- file.path(
#'         tiff_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'
#'     # Load and downsample
#'     s2 <- terra::aggregate(terra::rast(band_files), fact = 5)
#'
#'     # Generate seed patterns
#'     seeds <- manual_grid(
#'         s2,
#'         compactness = 0.1,
#'         r = 4, g = 3, b = 1
#'     )
#'
#'     # Run snic
#'     seg <- snic(s2, seeds, compactness = 0.1)
#'
#'     # Visualise one pattern
#'     snic_plot(
#'         s2,
#'         r = 4, g = 3, b = 1,
#'         stretch = "lin",
#'         seeds = seeds,
#'         seg = seg
#'     )
#' }
#' }
manual_grid <- function(img,
                        seeds = NULL,
                        compactness = 1.0,
                        ...) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop("Package 'terra' must be installed to handle SpatRaster input", call. = FALSE)
    }

    if (!inherits(img, "SpatRaster")) {
        stop("argument 'img' must be a 'SpatRaster' object", call. = FALSE)
    }

    if (!interactive()) {
        stop("'manual_grid()' can only be used in an interactive R session", call. = FALSE)
    }

    # Check and prepare seeds to integer matrix
    if (!is.null(seeds)) {
        seeds <- .prepare_seeds(seeds)
    }

    message("Left-click to add points; press ESC or right-click to stop.")

    snic_plot(img, seeds = seeds, ...)

    repeat {
        p <- locator(n = 1)
        if (is.null(p)) break

        r <- terra::rowFromY(img, p$y)
        c <- terra::colFromX(img, p$x)
        if (is.na(r) || is.na(c)) next

        seeds <- rbind(seeds, cbind(r = r, c = c))
        seg <- snic(img, seeds = seeds, compactness = compactness)

        snic_plot(img, seeds = seeds, seg = seg, ...)
    }

    invisible(seeds)
}

#' @rdname snic_grid
#' @export
count_seeds <- function(img, spacing, padding = NULL) {
    params <- .prepare_grid_args(img, spacing, padding, "count_seeds")
    prod(.count_seeds(params$h, params$w, params$spacing, params$padding))
}
