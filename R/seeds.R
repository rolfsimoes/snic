#' Grid SNIC seeding
#'
#' Place seeds on a regular grid, discarding any sites that fall on invalid
#' pixels (those with `NA` values).
#'
#' @param img Image data. For the `array` method this should be numeric or
#'   integer with 3 (height, width, bands) dimensions. For the `SpatRaster`
#'   method (from `terra`), the dimensions are inferred automatically.
#' @param grid_step Positive integer spacing between grid lines (in pixels).
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A two-column integer matrix `(row, column)` giving the 1-based pixel
#' coordinates of the selected seeds.
#'
#' @export
snic_seeds_grid <- function(img, grid_step = 10L, ...) {
    UseMethod("snic_seeds_grid")
}

#' @rdname snic_seeds_grid
#' @export
snic_seeds_grid.array <- function(img, grid_step = 10L, ...) {
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

    if (!is.integer(grid_step)) {
        grid_step <- as.integer(grid_step)
    }

    result <- .Call(
        C_snic_seeds_grid,
        img,
        grid_step
    )
}

#' @rdname snic_seeds_grid
#' @export
snic_seeds_grid.SpatRaster <- function(img, grid_step = 10L, ...) {
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

    if (!is.integer(grid_step)) {
        grid_step <- as.integer(grid_step)
    }

    result <- .Call(
        C_snic_seeds_grid,
        img_mtx,
        grid_step,
        PACKAGE = "snic"
    )
}

#' @rdname snic_seeds_grid
#' @export
snic_seeds_grid.default <- function(img, grid_step = 10L, ...) {
    stop("Unsupported input type '", class(img)[1],
        "'. SNIC seeding currently supports array and SpatRaster inputs only.",
        call. = FALSE
    )
}
