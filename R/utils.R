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

#' SNIC superpixel segmentation
#'
#' This function provides the R interface to the SNIC (Simple Non-Iterative
#' Clustering) segmentation algorithm implemented in C++.
#'
#' @param img Numeric array of dimensions \code{(height, width, bands)}
#'   representing the input image. The array may be stored in either
#'   row-major (\code{"C"}) or column-major (\code{"F"}) order, as
#'   specified by the \code{order} parameter. In row-major order, a pixel
#'   at position \code{(r, c)} and band \code{b} is located at
#'   \code{img[c + r * width + b * (width * height)]}. The array must have
#'   exactly three dimensions, each strictly positive.
#' @param seeds Two-column data frame (or matrix-like object) with columns
#'   \code{r} (row) and \code{c} (column) specifying the seed coordinates
#'   (1-based, R style). Coordinates must lie within image bounds.
#' @param compactness Numeric scalar specifying the compactness parameter.
#'   Must be finite and non-negative.
#' @param order Character scalar indicating the memory storage order of the
#'   input image: \code{"C"} for row-major or \code{"F"} for column-major.
#'
#' @return Integer vector (length \code{width * height}) with the same
#'   ordering as the input and defined by the \code{order} parameter. Pixels
#'   masked out remain \code{NA}. Segments are labeled from 1 to the number
#'   of seeds.
#' @keywords internal
#' @noRd

.is_interactive <- function() {
    forced <- getOption("snic.force_interactive", default = NULL)
    if (!is.null(forced)) {
        return(isTRUE(forced))
    }
    interactive()
}

.expand <- function(...) {
    expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}
