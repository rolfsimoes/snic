#' Simple Non-Iterative Clustering (SNIC) segmentation
#'
#' Segment an image into superpixels using the SNIC algorithm. This function
#' wraps a C++ implementation that works with any number of spectral bands.
#' The segmentation uses a 4-neighbour (von Neumann) connectivity.
#'
#' @param img Image data. For the `array` method this should be numeric or
#'   integer with 3 (height, width, bands) dimensions. For the `SpatRaster`
#'   method (from `terra`), the dimensions are inferred automatically.
#' @param seeds Optional integer matrix with two columns (row, column) giving
#'   1-based pixel coordinates for the initial seeds. If omitted, seeds are
#'   generated on a regular grid via [snic_seeds_grid()].
#' @param compactness Non-negative numeric value controlling the trade-off
#'   between color similarity and spatial proximity (default 10). Larger values
#'   encourage more spatially compact superpixels.
#' @param grid_step Positive integer spacing, expressed in pixels, used when
#'   generating default grid seeds. Defaults to 10.
#' @param ... Additional arguments passed to methods.
#'
#' @details
#' The `img` array must use R's standard column-major order with dimensions
#' `(rows, cols, bands)` if 3D, or `(rows, cols)` if 2D. This layout matches the
#' underlying C implementation of SNIC used by this package.
#'
#' @return
#' A matrix of superpixel labels with `height` rows and `width` columns.
#'
#' @export
snic <- function(img,
                 seeds = NULL,
                 compactness = 10,
                 grid_step = 10L,
                 ...) {
    UseMethod("snic")
}

#' @rdname snic
#' @export
snic.array <- function(img,
                       seeds = NULL,
                       compactness = 10,
                       grid_step = 10L,
                       ...) {
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

    if (length(dim(img)) != 3L) {
        stop(
            "Argument 'img' must have 3 (height, width, bands) dimensions",
            call. = FALSE
        )
    }

    if (is.integer(img)) {
        storage.mode(img) <- "double"
    }

    .Call(
        C_snic_snic,
        img,
        seeds,
        as.numeric(compactness),
        as.integer(grid_step)
    )
}

#' @rdname snic
#' @export
snic.SpatRaster <- function(img,
                            seeds = NULL,
                            compactness = 10,
                            grid_step = 10L,
                            ...) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop("Package 'terra' must be installed to handle SpatRaster input.",
            call. = FALSE
        )
    }

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

    img_mtx <- terra::values(img, mat = TRUE)
    if (is.null(img_mtx)) {
        stop("Unable to extract pixel values from the SpatRaster input.",
            call. = FALSE
        )
    }

    # Convert to array with (height, width, bands). Tested!
    .colmaj(img_mtx, terra::nrow(img), terra::ncol(img), terra::nlyr(img))

    result <- .Call(
        C_snic_snic,
        img_mtx,
        seeds,
        as.numeric(compactness),
        as.integer(grid_step)
    )

    if (isTRUE(getOption("snic.return_raster", FALSE))) {
        return(terra::rast(img, nlyrs = 1, vals = result))
    }

    result
}

#' @rdname snic
#' @export
snic.default <- function(img, ...) {
    stop("Unsupported input type '", class(img)[1],
        "'. SNIC currently supports array and SpatRaster inputs only.",
        call. = FALSE
    )
}
