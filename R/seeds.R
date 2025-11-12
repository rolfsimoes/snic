#' Seed helper utilities
#'
#' Developer-facing helpers for validating, coercing, and appending seed
#' coordinate tables. Seeds can be represented in three coordinate systems:
#' \itemize{
#'   \item pixel indices: \code{(r, c)}
#'   \item map units of the raster: \code{(x, y)}
#'   \item geographic coordinates: \code{(lat, lon)} in \code{EPSG:4326}
#' }
#'
#' Conversions between systems require the source raster to resolve extent,
#' resolution and CRS. Functions here keep column names consistent so that
#' downstream utilities (e.g., plotting or SNIC core) can dispatch correctly.
#'
#' @param seeds Data frame/matrix containing either \code{(r, c)},
#'   \code{(x, y)}, or \code{(lat, lon)} columns.
#' @param x A \code{\link[terra:SpatRaster-class]{SpatRaster}} used for
#'   coordinate conversions between systems.
#' @keywords internal
#' @name seeds_helpers
#' @rdname seeds_helpers
NULL

#' @rdname seeds_helpers
as_seeds_rc <- function(seeds, x) {
    switch_seeds(
        seeds,
        rc = seeds,
        wgs84 = wgs84_to_rc(x, seeds),
        xy = xy_to_rc(x, seeds)
    )
}

#' @rdname seeds_helpers
as_seeds_xy <- function(seeds, x) {
    switch_seeds(
        seeds,
        rc = rc_to_xy(x, seeds),
        wgs84 = wgs84_to_xy(x, seeds),
        xy = seeds
    )
}

#' @rdname seeds_helpers
as_seeds_wgs84 <- function(seeds, x) {
    switch_seeds(
        seeds,
        rc = rc_to_wgs84(x, seeds),
        wgs84 = seeds,
        xy = rc_to_wgs84(x, seeds)
    )
}

#' @rdname seeds_helpers
check_seeds <- function(seeds, param_name = "seeds") {
    UseMethod("check_seeds", seeds)
}

#' @rdname seeds_helpers
#' @export
check_seeds.data.frame <- function(seeds, param_name = "seeds") {
    switch_seeds(
        seeds,
        rc = seeds[, c("r", "c")],
        wgs84 = seeds[, c("lat", "lon")],
        xy = seeds[, c("x", "y")]
    )
}

#' @rdname seeds_helpers
#' @export
check_seeds.matrix <- function(seeds, param_name = "seeds") {
    if (!is.null(colnames(seeds))) {
        return(check_seeds.data.frame(as.data.frame(seeds), param_name))
    }
    if (ncol(seeds) != 2L) {
        stop(.msg("check_seeds_matrix_two_columns", param_name), call. = FALSE)
    }
    colnames(seeds) <- c("r", "c")
    return(as.data.frame(seeds))
}

#' @rdname seeds_helpers
#' @export
check_seeds.NULL <- function(seeds, param_name = "seeds") {
    data.frame(r = integer(0), c = integer(0))
}

#' @rdname seeds_helpers
#' @export
check_seeds.default <- function(seeds, param_name = "seeds") {
    check_seeds.data.frame(as.data.frame(seeds), param_name)
}

#' @rdname seeds_helpers
seeds_type <- function(seeds) {
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

#' @rdname seeds_helpers
switch_seeds <- function(seeds, ...) {
    switch(seeds_type(seeds),
        ...
    )
}

#' @rdname seeds_helpers
#' @param new_seed Single-row object to append to an existing seed set.
append_seed <- function(seeds, new_seed) {
    if (is.null(seeds)) {
        return(new_seed)
    }
    if (seeds_type(seeds) != seeds_type(new_seed)) {
        stop(.msg("seeds_type_mismatch"), call. = FALSE)
    }
    rbind(seeds, new_seed)
}

#' @rdname seeds_helpers
.seeds <- function(...) {
    data.frame(..., stringsAsFactors = FALSE)
}
