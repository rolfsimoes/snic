#' SNIC segmentation arrays
#'
#' Objects returned by [snic()] inherit from the `snic` S3 class. They behave
#' like plain 3D arrays while carrying hidden attributes with per-cluster
#' summaries produced by the SNIC algorithm.
#'
#' @name snic_class
NULL

#' @keywords internal
.new_snic <- function(x) {
    if (!is.array(x) || length(dim(x)) != 3L) {
        stop(.msg("snic_expected_array"), call. = FALSE)
    }
    structure(x, class = unique(c("snic", class(x))))
}

#' @keywords internal
.check_snic <- function(x, caller) {
    if (!inherits(x, "snic")) {
        stop(.msg("snic_expected_class", caller), call. = FALSE)
    }
    x
}

#' Segment feature summaries
#'
#' Access the feature means computed for every SNIC cluster. The resulting
#' matrix has one row per segment and one column per spectral band in the
#' source image.
#'
#' @param x A `snic` object, typically the direct output of [snic()].
#'
#' @return A numeric matrix with dimension `n_clusters x n_bands`.
#'
#' @export
snic_values <- function(x) {
    x <- .check_snic(x, "snic_values()")
    attr(x, "values", exact = TRUE)
}

#' Segment center coordinates
#'
#' Retrieve the spatial centroids `(r, c)` associated with each SNIC cluster.
#'
#' @inheritParams snic_values
#'
#' @return A numeric matrix with two columns named `r` and `c`.
#'
#' @export
snic_centers <- function(x) {
    x <- .check_snic(x, "snic_centers()")
    attr(x, "centers", exact = TRUE)
}

#' @export
print.snic <- function(x, ...) {
    vals <- attr(x, "values", exact = TRUE)
    ctrs <- attr(x, "centers", exact = TRUE)
    attr(x, "values") <- NULL
    attr(x, "centers") <- NULL
    class(x) <- setdiff(class(x), "snic")
    NextMethod("print")
    attr(x, "values") <- vals
    attr(x, "centers") <- ctrs
    invisible(.new_snic(x))
}
