#' @rdname snic_backends
#' @export
check_x.array <- function(x, param_name = "x") {
    if (length(dim(x)) != 3L) {
        stop(.msg("check_array_invalid_dims", param_name), call. = FALSE)
    }
    if (!is.numeric(x)) {
        stop(.msg("check_array_must_be_numeric", param_name), call. = FALSE)
    }
    x
}

#' @rdname snic_backends
#' @export
has_crs.array <- function(x) {
    FALSE
}

#' @rdname snic_backends
#' @export
wgs84_to_xy.array <- function(x, seeds_wgs84) {
    stop(.msg("array_no_projection_support"), call. = FALSE)
}

#' @rdname snic_backends
#' @export
xy_to_wgs84.array <- function(x, seeds_xy) {
    stop(.msg("array_no_projection_support"), call. = FALSE)
}

#' @rdname snic_backends
#' @export
xy_to_rc.array <- function(x, seeds_xy) {
    seeds <- .seeds(
        r = as.integer(nrow(x) - seeds_xy$y),
        c = as.integer(seeds_xy$x)
    )
    seeds$r[seeds$r < 0L | seeds$r > nrow(x)] <- NA
    seeds$c[seeds$c < 0L | seeds$c > ncol(x)] <- NA
    seeds
}

#' @rdname snic_backends
#' @export
rc_to_xy.array <- function(x, seeds_rc) {
    seeds <- .seeds(
        x = floor(seeds_rc$c) + 0.5,
        y = floor(nrow(x) - seeds_rc$r) + 0.5
    )
    seeds$y[seeds$y < 0 | seeds$y > nrow(x)] <- NA
    seeds$x[seeds$x < 0 | seeds$x > ncol(x)] <- NA
    seeds
}

#' @rdname snic_backends
#' @export
x_to_arr.array <- function(x) {
    x
}

#' @rdname snic_backends
#' @export
arr_to_x.array <- function(x, arr, names = NULL) {
    arr <- x_to_arr(arr)
    colnames(arr) <- names
    arr
}

#' @rdname snic_backends
x_bbox.array <- function(x) {
    dims <- dim(x)
    c(0L, dims[[2L]], 0L, dims[[1L]])
}
