#' Simple Non-Iterative Clustering (SNIC) segmentation
#'
#' Segment an image into superpixels using the SNIC algorithm.  This function
#' wraps a C++ implementation that works with any number of spectral bands.
#'
#' @param img A numeric matrix of dimension nPixels × nBands, where each row
#'   represents a pixel and each column a spectral band.
#' @param width Integer width of the image (number of columns in the original
#'   spatial raster).
#' @param height Integer height of the image (number of rows in the original
#'   spatial raster).
#' @param k Desired number of superpixels (clusters).
#' @param connectivity Neighborhood connectivity to use when growing clusters:
#'   either 4 (von Neumann) or 8 (Moore).
#' @return An integer vector of length equal to the number of pixels, giving the
#'   1-based cluster segment assigned to each pixel.
#' @useDynLib snic, .registration = TRUE, .fixes = "C_"
#' @export
snic <- function(img, width, height, k, connectivity = 4L) {
    stopifnot(is.matrix(img), is.numeric(img))
    stopifnot(is.numeric(width), length(width) == 1)
    stopifnot(is.numeric(height), length(height) == 1)
    stopifnot(is.numeric(k), length(k) == 1)
    stopifnot(is.numeric(connectivity), length(connectivity) == 1)
    # convert to integers
    width <- as.integer(width)
    height <- as.integer(height)
    k <- as.integer(k)
    connectivity <- as.integer(connectivity)
    if (!is.double(img)) {
        storage.mode(img) <- "double"
    }
    if (!connectivity %in% c(4L, 8L)) {
        stop("connectivity must be 4 or 8")
    }
    # ensure the matrix has the right number of rows
    n_pixels <- nrow(img)
    if (width * height != n_pixels) {
        stop("width * height must equal the number of rows in img")
    }
    # call the compiled C++ function
    res <- .Call(C_snic, img, width, height, k, connectivity)
    res
}
