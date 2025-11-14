#' Internal utilities for native calls and helpers
#'
#' Lightweight wrappers for calling into the C++ backend plus helper utilities
#' reused throughout the package.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{.call(fn_name, ...)} Executes the registered C++ symbol via
#'     \code{.Call} and wraps errors with friendly messages.
#'   \item \code{.expand(...)} A thin wrapper around \code{expand.grid()} that
#'     drops row attributes and prevents factor coercion.
#'   \item \code{.set_dim(x, dim)} Re-shapes \code{x} by delegating to the
#'     \code{snic_set_dim} native routine.
#'   \item \code{.snic_core(arr, seeds_rc, compactness)} The main C++ entry
#'     point for SNIC segmentation.
#'   \item \code{.polygonize(x)} Converts a segmentation raster into polygons.
#'   \item \code{.rast_tmpl(x)} Builds an empty \code{terra::rast()} template
#'     matching the array's footprint.
#' }
#'
#' @keywords internal
#' @name internal_utils
#' @rdname internal_utils
NULL

#' @rdname internal_utils
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

#' @rdname internal_utils
.expand <- function(...) {
    expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

#' @rdname internal_utils
.set_dim <- function(x, dim) {
    .call("snic_set_dim", x, as.integer(dim))
}

#' @rdname internal_utils
.snic_core <- function(arr, seeds_rc, compactness) {
    if ((!is.array(arr) || !is.numeric(arr) || length(dim(arr)) != 3)) {
        stop(.msg("img_must_be_numeric_array_three_dimensions"))
    }
    if (is.integer(arr)) {
        storage.mode(arr) <- "double"
    }

    if (.seeds_type(seeds_rc) != "rc") {
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

#' @rdname internal_utils
.polygonize <- function(x) {
    terra::as.polygons(x, dissolve = TRUE, na.rm = TRUE)
}

#' @rdname internal_utils
.rast_tmpl <- function(x) {
    terra::rast(
        nrows = nrow(x),
        ncols = ncol(x),
        xmin = 0,
        xmax = ncol(x),
        ymin = 0,
        ymax = nrow(x)
    )
}
