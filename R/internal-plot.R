#' Internal plotting utilities (developer documentation)
#'
#' Shared helpers for rendering arrays and raster objects via \pkg{terra}.
#' These functions are not exported; they encapsulate the common steps needed
#' for plotting SNIC images, seeds, and segments.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{.plot_core(x, plot_args)}
#'   Handles the low-level plotting of either a single band or an RGB composite.
#'
#'   \item \code{.plot_seeds(seeds_xy, x, plot_args, add)}
#'   Draws seed locations over an existing plot window.
#'
#'   \item \code{.plot_segments(x, plot_args, add)}
#'   Converts segmentation rasters into polygons and plots them with the
#'   supplied style overrides.
#' }
#'
#' @keywords internal
#' @name internal_plot_helpers
#' @rdname internal_plot_helpers
NULL

#' @rdname internal_plot_helpers
.plot_core <- function(x, plot_args) {
    if (!inherits(x, "SpatRaster")) {
        stop(.msg("plot_core_invalid_x"), call. = FALSE)
    }

    x <- .check_x(x)

    default_args <- list(
        band = 1L,
        r = NULL,
        g = NULL,
        b = NULL,
        col = grDevices::hcl.colors(128L, "Spectral"),
        stretch = "lin",
        smooth = FALSE,
        axes = FALSE
    )
    plot_args <- .modify_list(default_args, plot_args)

    band <- plot_args$band
    r <- plot_args$r
    g <- plot_args$g
    b <- plot_args$b
    col <- plot_args$col


    bands <- band
    if (length(r) && length(g) && length(b)) {
        bands <- c(r, g, b)
    }
    n_bands <- dim(x)[[3L]]
    if (length(bands) && any(is.na(bands) | bands < 1L | bands > n_bands)) {
        stop(.msg("raster_invalid_index", n_bands), call. = FALSE)
    }

    args <- setdiff(names(plot_args), c("band", "r", "g", "b", "col"))
    plot_args <- plot_args[args]
    if (length(bands) == 3L) {
        plot_args <- c(
            list(x, r = r, g = g, b = b),
            plot_args
        )
        do.call(terra::plotRGB, plot_args)
    } else {
        plot_args <- c(
            list(x, band, col = col, legend = FALSE),
            plot_args
        )
        do.call(terra::plot, plot_args)
    }

    invisible(NULL)
}

#' @rdname internal_plot_helpers
.plot_seeds <- function(seeds_xy, x, plot_args, add) {
    default_args <- list(pch = 16, col = "#00FFFF", cex = 1)
    plot_args <- .modify_list(default_args, plot_args)

    if (!add) {
        op <- graphics::par(mar = c(0, 0, 0, 0))
        on.exit(graphics::par(op), add = TRUE)
        bbox <- .x_bbox(x)
        xlim <- c(bbox[[1L]], bbox[[2L]])
        ylim <- c(bbox[[3L]], bbox[[4L]])
        graphics::plot.new()
        graphics::plot.window(xlim = xlim, ylim = ylim)
    }

    plot_args <- c(
        list(x = seeds_xy$x, y = seeds_xy$y),
        plot_args
    )

    do.call(graphics::points, plot_args)
}

#' @rdname internal_plot_helpers
.plot_segments <- function(x, plot_args, add) {
    default_args <- list(border = "#FFD700", col = NA, lwd = 0.6)
    plot_args <- .modify_list(default_args, plot_args)
    plot_args$add <- add

    # convert to SpatRaster
    if (!inherits(x, "SpatRaster")) {
        x <- .arr_to_x(.rast_tmpl(x), .x_to_arr(x))
    }

    seg <- .polygonize(x)

    plot_args <- .modify_list(plot_args, list(x = seg))
    do.call(terra::plot, plot_args)
}
