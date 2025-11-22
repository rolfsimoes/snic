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
#'   \item \code{.polygonize(x)} Converts a segmentation raster into polygons.
#'   \item \code{.rast_tmpl(x)} Builds an empty \code{terra::rast()} template
#'     matching the array's footprint.
#'   \item \code{.modify_list(x, y)} Modifies x list using named entries from y
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

#' @rdname internal_utils
.modify_list <- function(x, y) {
    stopifnot(!is.null(names(y)))
    for (p in names(y)) {
        if (nzchar(p)) {
            x[[p]] <- y[[p]]
        }
    }
    x
}
