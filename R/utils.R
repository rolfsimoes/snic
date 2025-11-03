#' SNIC superpixel segmentation
#'
#' This function provides the R interface to the SNIC (Simple Non-Iterative
#' Clustering) segmentation algorithm implemented in C++.
#'
#' @param img Numeric array of dimensions \code{(height, width, bands)} representing
#'   the input image. The array may be stored in either row-major (\code{"C"}) or
#'   column-major (\code{"F"}) order, as specified by the \code{order} parameter.
#'   In row-major order, a pixel at position \code{(r, c)} and band \code{b} is located at
#'   \code{img[c + r * width + b * (width * height)]}. The array must have exactly
#'   three dimensions, each strictly positive.
#' @param seeds Integer matrix with two columns \code{(row, column)} specifying the
#'   seed coordinates (1-based, R style). Coordinates must lie within image
#'   bounds.
#' @param compactness Numeric scalar specifying the compactness parameter.
#'   Must be finite and non-negative.
#' @param order Character scalar indicating the memory storage order of the
#'   input image: \code{"C"} for row-major or \code{"F"} for column-major.
#'
#' @return Integer vector (length \code{width * height}) with the same ordering
#'   as the input and defined by the \code{order} parameter. Pixels masked out
#'   remain \code{NA}. Segments are labelled from 1 to the number of seeds.
#' @keywords internal
#' @noRd
.snic <- function(img, seeds, compactness, order = c("C", "F")) {
    order <- match.arg(order)
    if (is.integer(img)) {
        storage.mode(img) <- "double"
    }
    .Call(
        C_snic_snic,
        img,
        seeds,
        as.numeric(compactness),
        order
    )
}

.set_dim <- function(x, new_dim) {
    .Call(
        C_snic_set_dim,
        x,
        as.integer(new_dim)
    )
}

.snic_count_seeds <- function(h, w, spacing, padding) {
    if (length(spacing) == 1) {
        spacing <- c(spacing, spacing)
    }
    if (length(padding) == 1) {
        padding <- c(padding, padding)
    }
    n_r <- floor((h - 2 * padding[[1]] - 1) / spacing[[1]]) + 1L
    n_c <- floor((w - 2 * padding[[2]] - 1) / spacing[[2]]) + 1L
    c(n_r, n_c)
}

# Internal utility: normalize seed matrices
.prepare_seeds <- function(seeds) {
    if (is.null(seeds)) {
        stop("argument 'seeds' cannot be NULL", call. = FALSE)
    }

    # Coerce to a matrix if needed (data.frame, tibble, etc.)
    if (!is.matrix(seeds) && !is.array(seeds)) {
        seeds <- as.matrix(seeds)
    }

    # Must have at least two columns
    if (ncol(seeds) < 2L) {
        stop("argument 'seeds' must have at least two columns", call. = FALSE)
    }

    # Check or assign column names
    if (is.null(colnames(seeds))) {
        if (ncol(seeds) != 2L) {
            stop("argument 'seeds' must be a matrix with two columns (row, column)")
        }
        # Assume (r, c) if no names provided
        colnames(seeds) <- c("r", "c")
    }

    # Require both expected names
    if (!all(c("r", "c") %in% colnames(seeds))) {
        stop("argument 'seeds' must include column names 'r' (row) and 'c' (column)", call. = FALSE)
    }

    if (nrow(seeds) == 0L) {
        stop("argument 'seeds' must contain at least one coordinate", call. = FALSE)
    }

    # Reorder columns explicitly to (r, c)
    seeds <- seeds[, c("r", "c"), drop = FALSE]

    # Convert to integer
    if (!is.integer(seeds)) {
        seeds <- round(seeds)
        storage.mode(seeds) <- "integer"
    }

    seeds
}

.prepare_plot_seeds <- function(seeds, h, w) {
    if (is.null(seeds)) {
        return(NULL)
    }

    seeds <- .prepare_seeds(seeds)

    if (any(seeds[, 1L] < 1L | seeds[, 1L] > h |
        seeds[, 2L] < 1L | seeds[, 2L] > w)) {
        stop("argument 'seeds' must lie within the image bounds", call. = FALSE)
    }

    seeds
}

.prepare_grid_args <- function(img, spacing, padding, fn_name) {
    h <- nrow(img)
    w <- ncol(img)
    if (length(h) != 1L || length(w) != 1L || is.na(h) || is.na(w) || h <= 0 || w <= 0) {
        stop(
            sprintf("Argument 'img' for %s() must have at least one row and one column.", fn_name),
            call. = FALSE
        )
    }
    if (length(spacing) == 0L) {
        stop(sprintf("Argument 'spacing' for %s() cannot be empty.", fn_name), call. = FALSE)
    }
    if (!is.numeric(spacing)) {
        stop(sprintf("Argument 'spacing' for %s() must be numeric.", fn_name), call. = FALSE)
    }
    if (any(!is.finite(spacing))) {
        stop(sprintf("Argument 'spacing' for %s() must contain only finite values.", fn_name), call. = FALSE)
    }
    if (any(spacing <= 0)) {
        stop(sprintf("Argument 'spacing' for %s() must be strictly positive.", fn_name), call. = FALSE)
    }
    if (length(spacing) == 1L) {
        spacing <- rep(spacing, 2L)
    } else if (length(spacing) != 2L) {
        stop(
            sprintf("Argument 'spacing' for %s() must have length 1 or 2.", fn_name),
            call. = FALSE
        )
    }
    if (is.null(padding)) {
        padding <- spacing / 2
    } else {
        if (!is.numeric(padding)) {
            stop(sprintf("Argument 'padding' for %s() must be numeric.", fn_name), call. = FALSE)
        }
        if (any(!is.finite(padding))) {
            stop(sprintf("Argument 'padding' for %s() must contain only finite values.", fn_name), call. = FALSE)
        }
        if (length(padding) == 1L) {
            padding <- rep(padding, 2L)
        } else if (length(padding) != 2L) {
            stop(
                sprintf("Argument 'padding' for %s() must have length 1 or 2.", fn_name),
                call. = FALSE
            )
        }
    }
    if (any(padding < 0)) {
        stop(sprintf("Argument 'padding' for %s() must be non-negative.", fn_name), call. = FALSE)
    }
    if (padding[[1]] >= h / 2 || padding[[2]] >= w / 2) {
        stop(
            sprintf("Argument 'padding' for %s() leaves no room for seed placement.", fn_name),
            call. = FALSE
        )
    }
    list(
        h = h,
        w = w,
        spacing = spacing,
        padding = padding
    )
}
