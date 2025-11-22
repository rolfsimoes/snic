#' Internal grid utilities (developer documentation)
#'
#' These functions implement the core logic for generating seed coordinates
#' used in SNIC grid-based seeding. They are not exported and should not be
#' called directly by users. All grid functions operate in pixel-index space
#' (row/column). CRS-aware conversion is handled elsewhere.
#'
#' @section Functions:
#' \itemize{
#'
#'   \item \code{.check_grid_args(x, spacing, padding)}
#'   Validates input dimensions and parameters. Ensures:
#'   \itemize{
#'     \item \code{x} has positive height and width,
#'     \item \code{spacing} is numeric of length 2 and greater than 1,
#'     \item \code{padding} is numeric of length 2 and non-negative,
#'     \item padding does not eliminate all valid placement area.
#'   }
#'
#'   \item \code{.grid_count_seeds(x, spacing, padding)}
#'   Computes the number of grid points in each dimension that fit within the
#'   interior region defined by \code{padding}.
#'
#'   \item \code{.grid_rect(x, spacing, padding)}
#'   Generates a rectangular grid of seed positions evenly spaced across the
#'   available region.
#'
#'   \item \code{.grid_diamond(x, spacing, padding)}
#'   Generates a rectangular grid and a second grid offset diagonally by
#'   half the spacing, producing a diamond pattern. Boundary checks ensure
#'   offset points remain valid.
#'
#'   \item \code{.grid_hex(x, spacing, padding)}
#'   Similar to \code{.grid_diamond}, but applies axis-dependent spacing to
#'   approximate a hexagonal tiling geometry.
#'
#'   \item \code{.grid_random(x, spacing, padding)}
#'   Places \code{prod(.grid_count_seeds(...))} uniformly sampled seed positions
#'   inside the padded region. Sampling is without replacement.
#'
#'   \item \code{.grid_manual(x, snic_args, snic_plot_args)}
#'   Interactive seeding. Displays an image and iteratively updates seeds
#'   based on mouse clicks. Re-runs SNIC and re-plots after each update.
#'   Intended for exploratory inspection, not automated workflows.
#' }
#'
#' @keywords internal
#' @name internal_grid_helpers
#' @rdname internal_grid_helpers
NULL

#' @rdname internal_grid_helpers
.check_grid_args <- function(x, spacing, padding) {
    h <- nrow(x)
    w <- ncol(x)
    if (is.null(h) || is.null(w) || h < 1L || w < 1L) {
        stop(.msg("grid_img_min_dimensions"), call. = FALSE)
    }
    if (!is.numeric(spacing)) {
        stop(.msg("grid_spacing_numeric"), call. = FALSE)
    }
    if (length(spacing) != 2L) {
        stop(.msg("grid_spacing_length"), call. = FALSE)
    }
    if (any(!is.finite(spacing))) {
        stop(.msg("grid_spacing_finite"), call. = FALSE)
    }
    if (any(spacing <= 1)) {
        stop(.msg("grid_spacing_greater_than_one"), call. = FALSE)
    }
    if (!is.numeric(padding)) {
        stop(.msg("grid_padding_numeric"), call. = FALSE)
    }
    if (length(padding) != 2L) {
        stop(.msg("grid_padding_length"), call. = FALSE)
    }
    if (any(!is.finite(padding))) {
        stop(.msg("grid_padding_finite"), call. = FALSE)
    }
    if (any(padding < 0)) {
        stop(.msg("grid_padding_non_negative"), call. = FALSE)
    }
    if (padding[[1]] >= h / 2 || padding[[2]] >= w / 2) {
        stop(.msg("grid_padding_no_space"), call. = FALSE)
    }

    invisible(NULL)
}

#' @rdname internal_grid_helpers
.grid_count_seeds <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)
    h <- nrow(x)
    w <- ncol(x)
    n_rows <- floor((h - 2 * padding[[1]] - 1) / spacing[[1]]) + 1L
    n_cols <- floor((w - 2 * padding[[2]] - 1) / spacing[[2]]) + 1L

    as.integer(c(n_rows, n_cols))
}

#' @rdname internal_grid_helpers
.grid_rect <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    size <- .grid_count_seeds(x, spacing, padding)
    if (any(!is.finite(size))) {
        stop(.msg("grid_unable_determine_seed_counts"), call. = FALSE)
    }
    if (any(size < 1L)) {
        stop(.msg("grid_no_valid_seed_positions"), call. = FALSE)
    }

    h <- nrow(x)
    w <- ncol(x)
    r0 <- padding[[1]] + 1
    r1 <- h - padding[[1]]
    c0 <- padding[[2]] + 1
    c1 <- w - padding[[2]]

    if (size[[1]] == 1L) {
        rows <- mean(c(r0, r1))
    } else {
        rows <- seq(r0, r1, length.out = size[[1]])
    }
    if (size[[2]] == 1L) {
        cols <- mean(c(c0, c1))
    } else {
        cols <- seq(c0, c1, length.out = size[[2]])
    }

    .expand(r = rows, c = cols)
}

#' @rdname internal_grid_helpers
.grid_diamond <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    h <- nrow(x)
    w <- ncol(x)
    spacing <- spacing * sqrt(2)

    base <- .grid_rect(x, spacing, padding)
    shifted <- .seeds(
        r = base$r + spacing[[1]] / 2,
        c = base$c + spacing[[2]] / 2
    )

    keep <- shifted$r <= (h - padding[[1]]) &
        shifted$c <= (w - padding[[2]])
    shifted <- shifted[keep, , drop = FALSE]

    rbind(base, shifted)
}

#' @rdname internal_grid_helpers
.grid_hex <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    h <- nrow(x)
    w <- ncol(x)
    spacing <- spacing * c(1, sqrt(3))

    base <- .grid_rect(x, spacing, padding)
    shifted <- .seeds(
        r = base$r + spacing[[1]] / 2,
        c = base$c + spacing[[2]] / 2
    )
    keep <- shifted$r <= (h - padding[[1]]) &
        shifted$c <= (w - padding[[2]])
    shifted <- shifted[keep, , drop = FALSE]

    rbind(base, shifted)
}

#' @rdname internal_grid_helpers
.grid_random <- function(x, spacing, padding) {
    stopifnot(length(spacing) == 2L)
    stopifnot(length(padding) == 2L)

    n <- prod(.grid_count_seeds(x, spacing, padding))
    if (!is.finite(n)) {
        stop(.msg("grid_unable_determine_seed_counts"), call. = FALSE)
    }
    if (n < 1L) {
        stop(.msg("grid_no_valid_seed_positions"), call. = FALSE)
    }

    h <- nrow(x)
    w <- ncol(x)
    inner_dims <- as.integer(round(c(h, w) - 2 * padding))
    area <- prod(inner_dims)

    samples <- sample.int(area, n, replace = FALSE)
    s <- arrayInd(samples, inner_dims)

    .seeds(r = s[, 1L] + padding[[1]], c = s[, 2L] + padding[[2]])
}

#' @rdname internal_grid_helpers
.grid_manual <- function(x, snic_args, plot_args) {
    if (!dev.interactive(TRUE)) {
        stop(.msg("manual_grid_interactive_only"), call. = FALSE)
    }

    default_snic_args <- list(seeds = NULL, compactness = 0.5)
    snic_args <- .modify_list(default_snic_args, snic_args)

    default_plot_args <- list(
        stretch = "lin",
        seeds_plot_args = list(
            pch = 4, col = "#FFFF00", cex = 1
        ),
        seg_plot_args = list(
            border = "#FFFF00", col = NA, lwd = 0.4
        )
    )
    plot_args <- .modify_list(default_plot_args, plot_args)

    message(.msg("manual_grid_instructions"))

    snic_args$seeds <- .seeds_check(snic_args$seeds)
    snic_args$seeds <- as_seeds_xy(snic_args$seeds, x)
    snic_args <- .modify_list(snic_args, list(x = x))
    if (nrow(snic_args$seeds)) {
        plot_args$seeds <- snic_args$seeds
        plot_args$seg <- do.call(snic, snic_args)
    }
    plot_args <- .modify_list(
        plot_args, list(x = x)
    )
    do.call(snic_plot, plot_args)

    repeat {
        p <- graphics::locator(n = 1)
        if (is.null(p)) break

        new_seed <- .seeds(x = p$x, y = p$y) # in x CRS
        if (any(is.na(.xy_to_rc(x, new_seed)))) next # outside image

        snic_args$seeds <- .append_seed(snic_args$seeds, new_seed)
        plot_args$seg <- do.call(snic, snic_args)
        plot_args$seeds <- snic_args$seeds
        do.call(snic_plot, plot_args)
    }

    as_seeds_rc(snic_args$seeds, x)
}
