gamma_correction <- function(rgb, min_value, max_value) {
    if (!is.matrix(rgb)) stop("Input must be a matrix.")
    if (ncol(rgb) < 3) stop("Input must have at least 3 columns: R, G, B.")
    if (ncol(rgb) > 3) warning("Input has more than 3 columns. Only first 3 (R, G, B) will be used.")

    # Safety check for normalization range
    if (any(min_value >= max_value)) {
        stop("Each element in 'min_value' must be strictly less than the corresponding element in 'max_value'.")
    }

    rgb <- rgb[, 1:3, drop = FALSE]

    # Input values need to be scale
    rgb <- sweep(rgb, 2, min_value, "-")
    rgb <- sweep(rgb, 2, max_value - min_value, "/")

    # Piecewise function v(t)
    # Source: Bruce Lindbloom, "RGB to XYZ"
    is_na <- is.na(rgb)
    idx <- rgb <= 0.04045
    idx[is_na] <- FALSE
    rgb[idx] <- rgb[idx] / 12.92
    # otherwise
    idx[is_na] <- TRUE
    rgb[!idx] <- ((rgb[!idx] + 0.055) / 1.055)^2.4
    rgb
}

#' Convert sRGB values to the CIE XYZ colour space.
#'
#' @param rgb Numeric matrix with at least three columns giving R, G, B values.
#' @param min_value Numeric vector of length three giving the minimum value for each channel.
#' @param max_value Numeric vector of length three giving the maximum value for each channel.
#' @param M Conversion matrix from linear sRGB to XYZ (rows X, Y, Z).
#' @return A numeric matrix with three columns (X, Y, Z).
#' @export
rgb2xyz <- function(rgb,
                    min_value = c(0, 0, 0),
                    max_value = c(10000, 10000, 10000),
                    M = matrix(c(
                        0.4124564, 0.3575761, 0.1804375,
                        0.2126729, 0.7151522, 0.0721750,
                        0.0193339, 0.1191920, 0.9503041
                    ), nrow = 3, byrow = TRUE)) {
    # Correct gamma
    rgb <- gamma_correction(rgb, min_value, max_value)

    # RGB -> XYZ conversion matrix (linear sRGB, D65 white point)
    # Source: Bruce Lindbloom, "RGB / XYZ Matrices"
    # http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
    t(M %*% t(rgb))
}

#' Convert CIE XYZ values to CIE Lab.
#'
#' @param xyz Numeric matrix with at least three columns giving X, Y, Z values.
#' @param ref_white Numeric vector giving the reference white (default D65).
#' @return A numeric matrix with columns L, a, b.
#' @export
xyz2lab <- function(xyz, ref_white = c(X = 0.95047, Y = 1.00000, Z = 1.08883)) {
    if (!is.matrix(xyz)) stop("Input must be a matrix.")
    if (ncol(xyz) < 3) stop("Input must have at least 3 columns: X, Y, Z.")
    if (ncol(xyz) > 3) warning("Input has more than 3 columns. Only first 3 (X, Y, Z) will be used.")

    xyz <- xyz[, 1:3, drop = FALSE]

    # Default: normalize by reference white (D65)
    fxyz <- sweep(xyz, 2, ref_white, "/")

    # Piecewise function f(t)
    # source: Bruce Lindbloom, "XYZ to Lab"
    # http://www.brucelindbloom.com/Eqn_XYZ_to_LAB.html
    epsilon <- 216 / 24389
    kapa <- 24389 / 27
    is_na <- is.na(fxyz)
    idx <- fxyz > epsilon
    idx[is_na] <- FALSE
    fxyz[idx] <- fxyz[idx]^(1 / 3)
    # otherwise
    idx[is_na] <- TRUE
    fxyz[!idx] <- (fxyz[!idx] * kapa + 16) / 116

    return(cbind(
        L = 116 * fxyz[, 2] - 16,
        a = 500 * (fxyz[, 1] - fxyz[, 2]),
        b = 200 * (fxyz[, 2] - fxyz[, 3])
    ))
}

#' Convert sRGB values directly to CIE Lab.
#'
#' @inheritParams rgb2xyz
#' @return A numeric matrix with columns L, a, b.
#' @export
rgb2lab <- function(rgb,
                    min_value = c(0, 0, 0),
                    max_value = c(10000, 10000, 10000)) {
    # Convert to XYZ
    xyz <- rgb2xyz(rgb, min_value, max_value)

    # Normalize by reference white (D65)
    xyz2lab(xyz)
}
