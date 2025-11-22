#' Internal seed utilities
#'
#' Low-level helpers shared across the seed conversion and validation stack.
#' These functions never touch disk or user I/O; they simply standardize how
#' seeds are checked, typed, and appended so that downstream helpers can focus
#' on the conversions.
#'
#' @section Components:
#' \describe{
#'   \item{\code{.seeds_check()}}{Generic plus methods that coerce arbitrary
#'   inputs into the required column pairs while emitting targeted errors.}
#'   \item{\code{.seeds_type()}}{Identifies which coordinate signature a seed
#'   object currently follows so that dispatchers can choose the right path.}
#'   \item{\code{.switch_seeds()}}{Wrapper around \code{switch()} that routes
#'   execution based on the detected type.}
#'   \item{\code{.append_seed()}}{Appends a single seed row after verifying the
#'   incoming and existing coordinate systems match.}
#'   \item{\code{.seeds()}}{Thin data.frame constructor with consistent defaults
#'   used throughout tests and helper code.}
#' }
#'
#' @param seeds A data frame, matrix, or \code{NULL} describing seeds.
#' @param new_seed Single-row object to append to an existing seed set.
#' @keywords internal
#' @name seeds_internal
NULL

#' @rdname seeds_internal
.seeds_check <- function(seeds) {
    UseMethod(".seeds_check", seeds)
}

#' @rdname seeds_internal
.seeds_check.data.frame <- function(seeds) {
    .switch_seeds(
        seeds,
        rc = seeds[, c("r", "c")],
        wgs84 = seeds[, c("lat", "lon")],
        xy = seeds[, c("x", "y")]
    )
}

#' @rdname seeds_internal
.seeds_check.matrix <- function(seeds) {
    if (!is.null(colnames(seeds))) {
        return(.seeds_check.data.frame(as.data.frame(seeds)))
    }
    if (ncol(seeds) != 2L) {
        stop(.msg("seeds_check_matrix_two_columns"), call. = FALSE)
    }
    colnames(seeds) <- c("r", "c")
    as.data.frame(seeds)
}

#' @rdname seeds_internal
.seeds_check.NULL <- function(seeds) {
    data.frame(r = integer(0), c = integer(0))
}

#' @rdname seeds_internal
.seeds_check.default <- function(seeds) {
    .seeds_check.data.frame(as.data.frame(seeds))
}

#' @rdname seeds_internal
.seeds_type <- function(seeds) {
    if (all(c("r", "c") %in% colnames(seeds))) {
        "rc"
    } else if (all(c("lon", "lat") %in% colnames(seeds))) {
        "wgs84"
    } else if (all(c("x", "y") %in% colnames(seeds))) {
        "xy"
    } else {
        stop(.msg("seeds_columns_lonlat_or_rc"), call. = FALSE)
    }
}

#' @rdname seeds_internal
.switch_seeds <- function(seeds, ...) {
    switch(.seeds_type(seeds),
        ...
    )
}

#' @rdname seeds_internal
.append_seed <- function(seeds, new_seed) {
    if (is.null(seeds)) {
        return(new_seed)
    }
    if (.seeds_type(seeds) != .seeds_type(new_seed)) {
        stop(.msg("seeds_type_mismatch"), call. = FALSE)
    }
    rbind(seeds, new_seed)
}

#' @rdname seeds_internal
.seeds <- function(...) {
    data.frame(..., stringsAsFactors = FALSE)
}
