#' @rdname snic_backends
#' @export
.check_x.array <- function(x, param_name = "x") {
    if (length(dim(x)) != 3L) {
        stop(.msg("check_array_invalid_dims", param_name), call. = FALSE)
    }
    if (!is.array(x) || !is.numeric(x)) {
        stop(.msg("check_array_must_be_numeric", param_name), call. = FALSE)
    }
    x
}

#' @rdname snic_backends
#' @export
.has_crs.array <- function(x) {
    FALSE
}

#' @rdname snic_backends
#' @export
.wgs84_to_xy.array <- function(x, seeds_wgs84) {
    stop(.msg("array_no_projection_support"), call. = FALSE)
}

#' @rdname snic_backends
#' @export
.xy_to_wgs84.array <- function(x, seeds_xy) {
    stop(.msg("array_no_projection_support"), call. = FALSE)
}

#' @rdname snic_backends
#' @export
.xy_to_rc.array <- function(x, seeds_xy) {
    stopifnot(.seeds_type(seeds_xy) == "xy")
    h <- nrow(x)
    w <- ncol(x)

    # used to get bottom and right coordinates inside the array domain
    eps <- 3 * .Machine$double.eps

    r_coord <- h - seeds_xy$y
    c_coord <- seeds_xy$x
    seeds <- .seeds(
        r = floor(r_coord - eps * abs(r_coord)) + 1L,
        c = floor(c_coord - eps * abs(c_coord)) + 1L
    )
    seeds$r[seeds$r < 1L | seeds$r > h] <- NA_integer_
    seeds$c[seeds$c < 1L | seeds$c > w] <- NA_integer_
    seeds
}

#' @rdname snic_backends
#' @export
.rc_to_xy.array <- function(x, seeds_rc) {
    stopifnot(.seeds_type(seeds_rc) == "rc")
    h <- nrow(x)
    w <- ncol(x)

    seeds <- .seeds(
        x = as.integer(seeds_rc$c - 1L) + 0.5,
        y = as.integer(h - seeds_rc$r) + 0.5
    )

    tol <- 3 * .Machine$double.eps
    seeds$y[seeds$y < -tol | seeds$y > h + tol] <- NA
    seeds$x[seeds$x < -tol | seeds$x > w + tol] <- NA
    seeds
}

#' @rdname snic_backends
#' @export
.x_to_arr.array <- function(x) {
    x
}

#' @rdname snic_backends
#' @export
.arr_to_x.array <- function(x, arr, names = NULL) {
    arr <- .x_to_arr(arr)
    if (!is.null(names)) {
        dimnames(arr)[[3]] <- names
    }
    arr
}

#' @rdname snic_backends
.x_bbox.array <- function(x) {
    dims <- dim(x)
    c(0L, dims[[2L]], 0L, dims[[1L]])
}

#' @rdname snic_backends
#' @export
.get_idx.array <- function(x, idx) {
    if (is.numeric(idx)) {
        return(idx)
    }
    bands <- dimnames(x)[[3]]
    if (is.null(bands)) {
        return(idx)
    }
    return(match(idx, bands))
}
