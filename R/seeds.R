#' Grid SNIC seeding
#'
#' Place seeds on a regular grid, discarding any sites that fall on invalid
#' pixels (those with `NA` values).
#'
#' @param img Image data. For the `matrix` method this should follow the same
#'   layout as [snic()], with one row per pixel. For the `SpatRaster` method
#'   (from `terra`), raster dimensions are inferred automatically.
#' @param width Image width (number of columns). Required for the `matrix`
#'   method.
#' @param height Image height (number of rows). Required for the `matrix`
#'   method.
#' @param step Positive integer spacing between grid lines (in pixels).
#' @param ... Additional arguments passed to methods.
#' @return matrix with two columns (row, column) giving the 1-based pixel
#'   coordinates of the selected seeds.
#' @export
snic_seeds_grid <- function(img, ...) {
    UseMethod("snic_seeds_grid")
}

#' @rdname snic_seeds_grid
#' @export
snic_seeds_grid.matrix <- function(img, width, height, step, ...) {
    call_seeds_grid(img, width, height, step)
}

#' @rdname snic_seeds_grid
#' @export
snic_seeds_grid.SpatRaster <- function(img, step, ...) {
    extra <- list(...)
    if (length(extra)) {
        unused <- names(extra)
        if (is.null(unused)) {
            unused <- rep("", length(extra))
        }
        if (any(!nzchar(unused))) {
            unnamed <- which(!nzchar(unused))
            unused[unnamed] <- paste0("<unnamed ", unnamed, ">")
        }
        stop(
            "Unused arguments for SpatRaster input: ",
            paste(unused, collapse = ", "),
            call. = FALSE
        )
    }
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop("terra package must be installed to handle SpatRaster input",
            call. = FALSE
        )
    }
    terra_ncol <- getFromNamespace("ncol", "terra")
    terra_nrow <- getFromNamespace("nrow", "terra")
    terra_values <- getFromNamespace("values", "terra")
    width <- terra_ncol(img)
    height <- terra_nrow(img)
    img_matrix <- terra_values(img, mat = TRUE)
    if (is.null(img_matrix)) {
        stop("Unable to extract values from the SpatRaster input",
            call. = FALSE
        )
    }
    call_seeds_grid(img_matrix, width, height, step)
}

call_seeds_grid <- function(img, width, height, step) {
    if (!is.matrix(img) || !is.numeric(img)) {
        stop("Argument 'img' must be a numeric matrix (pixels * bands)")
    }
    width <- check_positive_scalar(width, "width")
    height <- check_positive_scalar(height, "height")
    step <- check_positive_scalar(step, "step")
    n_pixels <- nrow(img)
    if (width * height != n_pixels) {
        stop("width * height must equal the number of rows in img")
    }
    if (!is.double(img)) {
        storage.mode(img) <- "double"
    }
    result <- .Call("C_seed_grid", img, width, height, step, PACKAGE = "snic")
    matrix(result, ncol = 2)
}

#' Plot SNIC seeds on image data
#'
#' Overlay SNIC seed coordinates on an image represented as a matrix or a
#' [terra::SpatRaster].
#'
#' @param x Image data.
#' @param y Ignored (included for compatibility with [graphics::plot()]).
#' @param ... For matrix inputs, additional arguments include `seeds`, `width`,
#'   `height`, `band`, `col`, `seed_col`, `seed_pch`, `seed_cex`, and any other
#'   parameters passed on to [graphics::image()]. For `SpatRaster` inputs the
#'   same seed-related arguments are recognised; remaining arguments are
#'   forwarded to [terra::plot()] when no seeds are supplied.
#' @return Invisibly, `NULL`.
#' @name plot_snic
NULL

#' @rdname plot_snic
#' @export
#' @method plot matrix
plot.matrix <- function(x, y, ...) {
    dots <- list(...)
    seeds <- dots$seeds
    width <- dots$width
    height <- dots$height
    if (is.null(width) || is.null(height)) {
        stop("`width` and `height` must be supplied when plotting SNIC seeds for matrix input")
    }
    band <- if (!is.null(dots$band)) dots$band else 1L
    col <- if (!is.null(dots$col)) dots$col else grDevices::grey.colors(64)
    seed_col <- if (!is.null(dots$seed_col)) dots$seed_col else "red"
    seed_pch <- if (!is.null(dots$seed_pch)) dots$seed_pch else 4
    seed_cex <- if (!is.null(dots$seed_cex)) dots$seed_cex else 1
    dots[c(
        "seeds", "width", "height", "band",
        "col", "seed_col", "seed_pch", "seed_cex"
    )] <- NULL
    plot_snic_matrix(
        img = x,
        width = width,
        height = height,
        seeds = seeds,
        band = band,
        col = col,
        seed_col = seed_col,
        seed_pch = seed_pch,
        seed_cex = seed_cex,
        image_args = dots
    )
}

plot_snic_matrix <- function(img,
                             width,
                             height,
                             seeds,
                             band = 1L,
                             col = grDevices::grey.colors(64),
                             seed_col = "red",
                             seed_pch = 4,
                             seed_cex = 1,
                             image_args = list()) {
    stopifnot(is.matrix(img), is.numeric(img))
    stopifnot(is.numeric(width), length(width) == 1)
    stopifnot(is.numeric(height), length(height) == 1)
    stopifnot(is.numeric(band), length(band) == 1)
    if (!is.null(seeds)) {
        if (!is.matrix(seeds) || ncol(seeds) != 2) {
            stop("seeds must be a matrix with two columns (row, column)")
        }
    }
    width <- as.integer(width)
    height <- as.integer(height)
    band <- as.integer(band)
    if (!is.double(img)) {
        storage.mode(img) <- "double"
    }
    pre_shaped <- nrow(img) == height && ncol(img) == width
    if (!pre_shaped) {
        n_pixels <- nrow(img)
        n_bands <- ncol(img)
        if (width * height != n_pixels) {
            stop("width * height must equal the number of rows in img")
        }
        if (band < 1L || band > n_bands) {
            stop("band must be between 1 and ", n_bands)
        }
        band_mat <- matrix(img[, band], nrow = height, ncol = width, byrow = TRUE)
    } else {
        if (band != 1L) {
            stop("band must be 1 for matrices shaped as height x width")
        }
        band_mat <- img
    }
    if (!is.null(seeds)) {
        if (!is.integer(seeds) && !all(seeds == as.integer(seeds))) {
            stop("seeds must contain integer coordinates")
        }
        seeds_mat <- matrix(as.integer(seeds), nrow = nrow(seeds), ncol = 2)
        if (nrow(seeds_mat) == 0L) {
            stop("seeds must contain at least one coordinate")
        }
        if (any(seeds_mat[, 1L] < 1L | seeds_mat[, 1L] > height |
            seeds_mat[, 2L] < 1L | seeds_mat[, 2L] > width)) {
            stop("seeds must lie within the image bounds")
        }
    }
    display_mat <- t(band_mat) # transpose so columns map to x-axis in image()
    default_args <- list(
        x = seq_len(width),
        y = seq_len(height),
        z = display_mat,
        col = col,
        xlab = "Column",
        ylab = "Row",
        asp = 1,
        useRaster = TRUE,
        ylim = c(height, 1)
    )
    combined_args <- utils::modifyList(default_args, image_args)
    do.call(graphics::image, combined_args)
    if (!is.null(seeds)) {
        graphics::points(seeds_mat[, 2L], seeds_mat[, 1L],
            pch = seed_pch, col = seed_col, cex = seed_cex
        )
    }
    invisible(NULL)
}
