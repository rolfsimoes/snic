.snic_plot_array <- function(x,
                             band = 1L,
                             r = NULL,
                             g = NULL,
                             b = NULL,
                             col = grDevices::hcl.colors(
                                 n = 128L, palette = "Spectral"
                             ),
                             stretch = "lin",
                             ...,
                             seeds = NULL,
                             seeds_plot_args = list(
                                 pch = 4, col = "red", cex = 1
                             )) {
    if (!is.array(x) || length(dim(x)) != 3L) {
        stop(.msg("plot_array_invalid_dims"), call. = FALSE)
    }
    if (!is.numeric(x)) {
        stop(.msg("plot_array_must_be_numeric"), call. = FALSE)
    }

    dims <- dim(x)
    height <- dims[1L]
    width <- dims[2L]
    nbands <- dims[3L]

    rgb_requested <- !is.null(r) || !is.null(g) || !is.null(b)

    if (rgb_requested) {
        if (is.null(r) || is.null(g) || is.null(b)) {
            stop(.msg("plot_rgb_params_missing"), call. = FALSE)
        }
        rgb <- as.integer(c(r, g, b))
        if (any(is.na(rgb))) {
            stop(.msg("plot_rgb_params_not_integer"), call. = FALSE)
        }
        if (any(rgb < 1L | rgb > nbands)) {
            stop(.msg("plot_rgb_invalid_index_array", nbands), call. = FALSE)
        }
        .plot_rgb_array(
            x,
            r = rgb[1L],
            g = rgb[2L],
            b = rgb[3L],
            stretch = stretch,
            ...
        )
    } else {
        band <- as.integer(band)
        if (length(band) != 1L || is.na(band)) {
            stop(.msg("plot_band_single_integer"), call. = FALSE)
        }
        if (band < 1L || band > nbands) {
            stop(.msg("plot_band_invalid_index_array", band, nbands),
                call. = FALSE
            )
        }
        .plot_array(x, band, col, ..., stretch = stretch)
    }

    # Allow seeds parameter NULL
    seeds <- .as_seeds_array(.as_seeds(seeds), x)

    if (nrow(seeds)) {
        if (is.null(seeds_plot_args)) {
            seeds_plot_args <- list()
        }
        seed_defaults <- list(pch = 4, col = "#FFFF00", cex = 1)
        seed_args <- utils::modifyList(seed_defaults, seeds_plot_args)
        do.call(
            graphics::points,
            c(
                list(x = seeds$c, y = seeds$r),
                seed_args
            )
        )
    }

    invisible(NULL)
}

.plot_rgb_array <- function(x, r, g, b, stretch, ...) {
    # TODO let user pass/modify parameters
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op))
    graphics::par(
        mai = rep(0, 4),
        mar = rep(0, 4),
        oma = rep(0, 4),
        bg = "black",
        plt = c(0, 1, 0, 1),
        xaxs = "i",
        yaxs = "i"
    )

    dims <- dim(x)
    h <- dims[1L]
    w <- dims[2L]

    r_arr <- .stretch_band(x[, , r, drop = TRUE], stretch)
    g_arr <- .stretch_band(x[, , g, drop = TRUE], stretch)
    b_arr <- .stretch_band(x[, , b, drop = TRUE], stretch)

    rgb_arr <- array(0, dim = c(h, w, 4L))
    rgb_arr[, , 1L] <- r_arr
    rgb_arr[, , 2L] <- g_arr
    rgb_arr[, , 3L] <- b_arr
    rgb_arr[, , 4L] <- 1 # TODO let user choose alpha?
    rgb_arr[is.nan(rgb_arr[, , ])] <- 0

    graphics::plot.new()
    graphics::plot.window(
        xlim = c(0, w),
        ylim = c(0, h),
        asp = 1
    )
    graphics::rasterImage(
        rgb_arr,
        xleft = 0,
        ybottom = 0,
        xright = w,
        ytop = h,
        interpolate = FALSE,
        ...
    )
}

.plot_array <- function(x, band, col, stretch, ...) {
    # TODO let user pass/modify parameters
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op))
    graphics::par(
        mar = rep(0, 4),
        oma = rep(0, 4),
        bg = "black",
        plt = c(0, 1, 0, 1),
        xaxs = "i",
        yaxs = "i"
    )

    x <- x[, , band, drop = TRUE]

    dims <- dim(x)
    h <- dims[1L]
    w <- dims[2L]

    img_arr <- .stretch_band(x, stretch)

    graphics::image(
        x = 0:w,
        y = 0:h,
        z = t(img_arr),
        col = col,
        asp = 1,
        useRaster = TRUE,
        ylim = c(h, 0),
        ...
    )
}

.stretch_band <- function(x, method) {
    if (method == "none") {
        rng <- range(x, na.rm = TRUE)
        return((x - rng[1]) / diff(rng))
    }
    if (method == "lin") {
        q <- stats::quantile(x, probs = c(0.02, 0.98), na.rm = TRUE)
        x[x < q[1]] <- q[1]
        x[x > q[2]] <- q[2]
        return((x - q[1]) / diff(q))
    }
    if (method == "sd") {
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        rng <- c(m - 2 * s, m + 2 * s)
        x <- pmin(pmax(x, rng[1]), rng[2])
        return((x - rng[1]) / diff(rng))
    }
    if (method == "hist") {
        p <- stats::ecdf(x[is.finite(x)])
        res <- p(x)
        dim(res) <- dim(x)
        return(res)
    }
    stop(.msg("stretch_method_invalid"), call. = FALSE)
}
