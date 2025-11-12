#' Internal utilities for native calls
#'
#' Lightweight wrappers for calling into the C++ backend plus helper utilities
#' reused throughout the package.
#'
#' @param fn_name Symbol name registered via `.Call`.
#' @param ... Additional arguments forwarded to the native routine or grid helper.
#' @param x Object to reshape when using `set_dim()`.
#' @param dim Target dimension vector supplied to `set_dim()`.
#' @keywords internal
#' @name utils_call_helpers
#' @rdname utils_call_helpers
.call <- function(fn_name, ...) {
    dots <- list(...)
    tryCatch(
        {
            do.call(.Call, c(fn_name, PACKAGE = "snic", dots))
        },
        error = function(e) {
            stop(.msg(conditionMessage(e)))
        }
    )
}

#' @rdname utils_call_helpers
expand <- function(...) {
    expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

#' @rdname utils_call_helpers
set_dim <- function(x, dim) {
    .call("snic_set_dim", x, as.integer(dim))
}

#' SNIC superpixel segmentation (core routine)
#'
#' Internal interface to the C++ implementation of SNIC (Simple Non-Iterative
#' Clustering). This function assumes that all spatial conversion, seed
#' preprocessing, and array layout normalization have already been performed by
#' higher-level wrappers such as \code{\link{snic}}, \code{snic_grid}, and
#' backend-specific methods.
#'
#' @param arr Numeric array of shape \code{(height, width, bands)} in
#'   column-major (R-native) memory order. All dimensions must be strictly
#'   positive. Integer arrays are automatically converted to double precision.
#'
#' @param seeds_rc Two-column object specifying seed pixel coordinates (1-based)
#'   in row/column index space. Must contain columns \code{r} and \code{c}.
#'   Coordinates must lie within image bounds. No spatial reference is used here
#'   — coordinate systems and projection handling occur upstream.
#'
#' @param compactness Non-negative numeric scalar controlling the trade-off
#'   between feature similarity and spatial proximity. Larger values encourage
#'   more spatially compact superpixels. The value is coerced to \code{numeric}.
#'
#' @return
#' A single-band integer array of shape \code{(height, width)} where each pixel
#' stores the superpixel (cluster) label assigned by SNIC. Labels are consecutive
#' integers starting at 1. No CRS or raster metadata are attached at this stage.
#'
#' @keywords internal
#' @noRd
snic_core <- function(arr, seeds_rc, compactness) {
    if ((!is.array(arr) || !is.numeric(arr) || length(dim(arr)) != 3)) {
        stop(.msg("img_must_be_numeric_array_three_dimensions"))
    }
    if (is.integer(arr)) {
        storage.mode(arr) <- "double"
    }

    if (seeds_type(seeds_rc) != "rc") {
        stop(.msg("seeds_invalid_type"))
    }

    if (!nrow(seeds_rc)) {
        stop(.msg("seeds_must_have_coordinates"))
    }
    seeds_rc <- as.matrix(round(seeds_rc))
    storage.mode(seeds_rc) <- "integer"

    compactness <- as.numeric(compactness)

    .call("snic_snic", arr, seeds_rc, compactness, "F")
}

#' Raster conversion helpers
#'
#' Internal helpers for converting plain arrays back into temporary rasters and
#' polygonizing segmentation results for plotting.
#'
#' @param x Raster-like object.
#' @keywords internal
#' @name snic_rast_helpers
#' @rdname snic_rast_helpers
as_plt_rast <- function(x,
                        band = 1L,
                        r = NULL,
                        g = NULL,
                        b = NULL,
                        maxcell = 100000L,
                        method = "mean") {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(.msg("terra_required"), call. = FALSE)
    }

    dims <- dim(x)
    h <- dims[[1]]
    w <- dims[[2]]
    n_bands <- dims[[3]]

    bands <- band
    if (!is.null(r) || !is.null(g) || !is.null(b)) {
        bands <- c(r, g, b)
    }

    if (any(bands < 1L | bands > n_bands)) {
        stop(.msg("raster_invalid_index"), call. = FALSE)
    }

    x <- terra::rast(
        x,
        nlyrs = length(bands),
        vals = terra::values(x[, , bands, drop = FALSE])
    )
    terra::ext(x) <- c(0, w, 0, h)
    terra::crs(x) <- ""

    n <- terra::ncell(x)
    if (n <= maxcell) {
        return(x)
    }

    agg_factor <- ceiling(sqrt(n / maxcell))
    terra::aggregate(x, fact = agg_factor, fun = method, na.rm = TRUE)
}

#' @rdname snic_rast_helpers
polygonize <- function(x) {
    terra::as.polygons(x, dissolve = TRUE, na.rm = TRUE)
}

rast_tmpl <- function(x) {
    terra::rast(
        nrows = nrow(x),
        ncols = ncol(x),
        xmin = 0,
        xmax = ncol(x),
        ymin = 0,
        ymax = nrow(x)
    )
}
