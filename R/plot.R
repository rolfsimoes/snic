#' @export
snic_plot_grid <- function(seeds, x, ..., add = FALSE) {
    seeds <- check_seeds(seeds)

    seeds_xy <- as_seeds_xy(seeds, x)
    plot_args <- list(...)
    plot_grid(seeds_xy, x, plot_args, add = add)
}

#' @export
snic_plot_segments <- function(x, ..., add = FALSE, maxcell = 100000L) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(.msg("terra_required"), call. = FALSE)
    }

    if (dim(x)[[3]] != 1L) {
        stop("raster segments must have one band")
    }

    plot_args <- list(...)

    # convert to SpatRaster
    if (!inherits(x, "SpatRaster")) {
        x <- arr_to_x(
            terra::rast(x), # template
            x_to_arr(x[, , bands, drop = FALSE])
        )
    }
    plot_segments(x, plot_args, add = add)
}

plot_core <- function(x, band, r, g, b, col, stretch, maxcell, ...) {
    if (!inherits(x, "SpatRaster")) {
        stop("argument 'x' is not a SpaceRaster")
    }

    x <- check_x(x)

    bands <- band
    if (!is.null(r) || !is.null(g) || !is.null(b)) {
        bands <- c(r, g, b)
    }
    n_bands <- dim(x)[[3L]]
    if (any(bands < 1L | bands > n_bands)) {
        stop(.msg("raster_invalid_index"), call. = FALSE)
    }

    if (length(bands) == 3L) {
        terra::plotRGB(
            x,
            r = r,
            g = g,
            b = b,
            mar = 0,
            smooth = FALSE,
            stretch = stretch,
            axes = FALSE,
            maxcell = maxcell,
            ...
        )
    } else {
        terra::plot(
            x, band,
            col = col,
            mar = 0,
            legend = FALSE,
            axes = FALSE,
            maxcell = maxcell,
            smooth = FALSE,
            stretch = stretch,
            ...
        )
    }

    invisible(NULL)
}

plot_grid <- function(seeds_xy, x, plot_args, add) {
    default_args <- list(pch = 4, col = "black", cex = 1)
    plot_args <- utils::modifyList(default_args, plot_args)

    if (!add) {
        oldpar <- graphics::par(mar = c(0, 0, 0, 0))
        on.exit(graphics::par(oldpar), add = TRUE)
        bbox <- x_bbox(x)
        xlim <- c(bbox[[1L]], bbox[[2L]])
        ylim <- c(bbox[[3L]], bbox[[4L]])
        graphics::plot.new()
        graphics::plot.window(xlim = xlim, ylim = ylim)
    }

    do.call(
        graphics::points,
        c(list(x = seeds_xy$x, y = seeds_xy$y), plot_args)
    )
}

plot_segments <- function(x, plot_args, add) {
    default_args <- list(border = "#FFFF00", col = NA, lwd = 0.4)
    plot_args <- utils::modifyList(default_args, plot_args)
    plot_args$add <- add

    seg <- polygonize(x)
    do.call(terra::plot, c(list(seg), plot_args))
}
