#' @keywords internal
.snic_new <- function(seg) {
    seg <- .check_x(seg)
    seg_attrs <- attributes(seg)
    attributes(seg) <- seg_attrs[!grepl("^snic\\.", names(seg_attrs))]
    seg_attrs <- seg_attrs[grepl("^snic\\.", names(seg_attrs))]
    names(seg_attrs) <- gsub("^snic\\.", "", names(seg_attrs))
    structure(c(list(seg = seg), seg_attrs), class = "snic")
}

#' @keywords internal
.snic_check <- function(x) {
    if (!inherits(x, "snic")) {
        stop(.msg("snic_expected_class"), call. = FALSE)
    }
    if (!is.list(x)) {
        stop(.msg("snic_expected_list"), call. = FALSE)
    }
    if (!"seg" %in% names(x)) {
        stop(.msg("snic_expected_seg"), call. = FALSE)
    }
    x
}

#' @keywords internal
.snic_seg <- function(x) {
    .check_x(x$seg)
}

#' @keywords internal
.snic_means <- function(x) {
    x$means
}

#' @keywords internal
.snic_centroids <- function(x) {
    x$centroids
}
