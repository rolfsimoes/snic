#' Simple Non-Iterative Clustering (SNIC) segmentation
#'
#' Segment an image into superpixels using the SNIC algorithm. This function
#' wraps a C++ implementation that works with any number of spectral bands.
#' The segmentation uses a 4-neighbour (von Neumann) connectivity.
#'
#' @param img Image data. For the `matrix` method this should be a numeric
#'   matrix with one row per pixel and one column per band. For the
#'   `SpatRaster` method (from `terra`), the raster dimensions are inferred
#'   automatically.
#' @param width Integer width of the image (number of columns in the original
#'   spatial raster). Required for the `matrix` method.
#' @param height Integer height of the image (number of rows in the original
#'   spatial raster). Required for the `matrix` method.
#' @param compactness Non-negative numeric value controlling the trade-off
#'   between color similarity and spatial proximity (default 10). Larger values
#'   encourage more spatially compact superpixels.
#' @param seeds Optional integer matrix with two columns (row, column) giving
#'   1-based pixel coordinates for the initial seeds. If omitted, seeds are
#'   generated on a regular grid via [snic_seeds_grid()].
#' @param grid_step Positive integer spacing, expressed in pixels, used when
#'   generating default grid seeds. Defaults to 10.
#' @param ... Additional arguments passed to methods.
#'
#' @details
#' The `img` argument follows the convention used by the `terra` package:
#' each row corresponds to a pixel (in raster order, from top-left to
#' bottom-right), and each column corresponds to a spectral band or channel.
#' Thus, the total number of rows must equal `width * height`.
#'
#' @return
#' A matrix of superpixel labels with `height` rows and `width` columns. When
#' flattened with `as.vector(t(result))`, the values match the raster ordering
#' used by [terra::values()].
#'
#' @export
snic <- function(img, ...) {
    UseMethod("snic")
}

#' @rdname snic
#' @export
snic.matrix <- function(img,
                        width,
                        height,
                        seeds = NULL,
                        compactness = 10,
                        grid_step = 10L,
                        ...) {
    call_snic(img, width, height, seeds, compactness, grid_step)
}

#' @rdname snic
#' @export
snic.SpatRaster <- function(img,
                            seeds = NULL,
                            compactness = 10,
                            grid_step = 10L,
                            ...) {
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

    if (!requireNamespace("terra", quietly = TRUE)) {
        stop("terra package must be installed to handle SpatRaster input",
            call. = FALSE
        )
    }
    terra_ncol <- getFromNamespace("ncol", "terra")
    terra_nrow <- getFromNamespace("nrow", "terra")
    terra_values <- getFromNamespace("values", "terra")
    width <- terra_ncol(img)
    height <- terra_nrow(img)
    img_matrix <- terra_values(img, mat = TRUE)
    if (is.null(img_matrix)) {
        stop("Unable to extract values from the SpatRaster input",
            call. = FALSE
        )
    }
    labels <- call_snic(img_matrix, width, height, seeds, compactness, grid_step)
    if (isTRUE(getOption("snic.return_raster", FALSE))) {
        return(terra::rast(img, nlyrs = 1, vals = as.vector(t(labels))))
    }
    labels
}

call_snic <- function(img,
                      width,
                      height,
                      seeds,
                      compactness,
                      grid_step) {
    if (!is.matrix(img) || !is.numeric(img)) {
        stop("Argument 'img' must be a numeric matrix (pixels * bands)")
    }
    width <- check_positive_scalar(width, "width")
    height <- check_positive_scalar(height, "height")
    compactness <- check_positive_scalar(
        compactness,
        "compactness",
        type = "numeric",
        allow_zero = TRUE
    )
    grid_step <- check_positive_scalar(grid_step, "grid_step")
    # ensure the matrix has the right number of rows
    n_pixels <- nrow(img)
    if (width * height != n_pixels) {
        stop("Number of pixels (nrow(img)) must equal width * height.")
    }
    if (!is.double(img)) {
        storage.mode(img) <- "double"
    }
    if (is.null(seeds)) {
        seeds <- snic_seeds_grid(img, width, height, grid_step)
    } else {
        if (!is.matrix(seeds) || ncol(seeds) != 2L) {
            stop(
                "Argument 'seeds' must be a 2-column integer ",
                "matrix (row, column)"
            )
        }
        if (anyNA(seeds) || any(!is.finite(seeds))) {
            stop("Argument 'seeds' contains NA or non-finite values")
        }
        if (!is.integer(seeds)) seeds <- as.integer(seeds)
        if (any(seeds[, 1L] < 1L | seeds[, 1L] > height |
            seeds[, 2L] < 1L | seeds[, 2L] > width)) {
            stop("Argument 'seeds' coordinates must lie within image bounds")
        }
    }
    seeds_mat <- matrix(seeds, nrow = nrow(seeds), ncol = 2)
    if (nrow(seeds_mat) == 0L) {
        stop("Argument 'seeds' must contain at least one coordinate")
    }
    result <- .Call("C_snic", img, width, height, seeds_mat, compactness, PACKAGE = "snic")
    matrix(result, nrow = height, ncol = width, byrow = TRUE)
}
