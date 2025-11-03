#' Simple Non-Iterative Clustering (SNIC) segmentation
#'
#' Segment an image into superpixels using the SNIC algorithm.
#' This function wraps a C++ implementation that operates on any number
#' of spectral bands and uses 4-neighbour (von Neumann) connectivity.
#'
#' @param img Image data. For the \code{array} method this should be a numeric
#'   or integer array with three dimensions \code{(height, width, bands)} in
#'   column-major order (R's native storage). For the
#'   \code{\link[terra:SpatRaster-class]{SpatRaster}} method (from \pkg{terra}),
#'   dimensions and ordering are inferred automatically.
#' @param seeds A two-column object specifying the initial seed coordinates.
#'   The expected structure is a numeric or integer matrix (or any object that
#'   can be coerced to such) with columns named \code{r} (row) and \code{c}
#'   (column), giving the 1-based pixel coordinates of each cluster centre.
#'   These seeds define the starting positions for region growth. They can be
#'   generated automatically using the functions in \code{\link{snic_grid}}
#'   (for example, \code{\link{snic_rect_grid}}, \code{\link{snic_diamon_grid}},
#'   \code{\link{snic_hex_grid}}, or \code{\link{snic_random_grid}}), or
#'   interactively using \code{\link{snic_manual_grid}}. Passing the seed matrix
#'   explicitly ensures reproducibility and full control over segmentation
#'   initialisation.
#' @param compactness Non-negative numeric value controlling the trade-off
#'   between feature similarity and spatial proximity (default = 10).
#'   Larger values produce more spatially compact clusters.
#' @param ... Reserved for future use. Currently ignored.
#'
#' @details
#' The algorithm performs clustering in an \eqn{(n + 2)}-dimensional space
#' that combines \eqn{n} spectral (or feature) dimensions with the two spatial
#' coordinates of each pixel. The \code{compactness} parameter balances spectral
#' similarity and spatial distance during region growth.
#'
#' The seed matrix defines the initial cluster centres, and each pixel is
#' assigned to the nearest seed under the SNIC distance metric. The output
#' assigns each pixel a unique superpixel label corresponding to its cluster.
#'
#' If no seeds are provided, the function stops with an informative
#' error. Seeds must be generated beforehand using one of the
#' \code{\link{snic_grid}} utilities to ensure consistency across seeding
#' strategies.
#'
#' @return
#' A single-band image (same height and width as \code{img}) where each pixel
#' value corresponds to its superpixel (cluster) identifier.
#'
#' @seealso
#' \code{\link{snic_grid}} for seed generation functions and
#' \code{\link{snic_count_seeds}} for estimating seed counts.
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
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
#'     s2 <- terra::aggregate(terra::rast(band_files), factor = 5)
#'
#'     seeds <- snic_rect_grid(s2, spacing = 10L, padding = 50L)
#'     seg <- snic(s2, seeds = seeds, compactness = 0.25)
#'
#'     snic_plot(
#'         s2,
#'         r = 4, g = 3, b = 1,
#'         stretch = "lin",
#'         seeds = seeds,
#'         seg = seg,
#'         seg_plot_args = list(
#'             border = "#FFFF00",
#'             col = NA,
#'             lwd = 0.4
#'         )
#'     )
#' }
#' @export
snic <- function(img,
                 seeds,
                 compactness = 1.0,
                 ...) {
    UseMethod("snic")
}

#' @rdname snic
#' @export
snic.array <- function(img,
                       seeds,
                       compactness = 1.0,
                       ...) {
    if (!inherits(img, "array")) {
        stop("Unsupported input type '", class(img)[[1L]], call. = FALSE)
    }

    dims <- dim(img)
    if (length(dims) != 3L) {
        stop(
            "argument 'img' must have 3 dimensions (height, width, bands)",
            call. = FALSE
        )
    }

    # Check and prapare seeds to integer matrix
    seeds <- .prepare_seeds(seeds)

    extra <- list(...)
    if (length(extra)) {
        bad_args <- names(extra)
        bad_args[!nzchar(bad_args)] <-
            paste0("<unnamed ", seq_len(sum(!nzchar(bad_args))), ">")
        stop("Unused arguments for array input: ",
            paste(bad_args, collapse = ", "),
            call. = FALSE
        )
    }

    # Duplicate memory to avoid side-effects
    img_data <- array(img, dim = dims)

    # Keep column-major storage ordering
    img_data <- .snic(img_data, seeds, compactness, order = "F")

    # Set dimensions (height, width, bands) in-place
    .set_dim(img_data, c(dims[[1L]], dims[[2L]], 1L))
}
