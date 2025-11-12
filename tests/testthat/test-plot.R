test_that("snic_plot.array validates inputs", {
    skip_if_not_installed("terra")
    img2d <- array(runif(4), dim = c(2, 2))
    expect_error(
        snic_plot(img2d),
        "'x' must be a 3D array with dimensions \\(height, width, bands\\)"
    )

    img_chr <- array(letters[1:8], dim = c(2, 2, 2))
    expect_error(
        snic_plot(img_chr),
        "'x' must be a numeric array"
    )
})

test_that("snic_plot.array validates band and RGB selection", {
    skip_if_not_installed("terra")
    img <- array(runif(8), dim = c(2, 2, 2))
    expect_error(
        snic_plot(img, band = 3L),
        "Invalid 'band' index \\(3\\). Array has 2 bands\\."
    )
    expect_error(
        snic_plot(img, r = 1L, g = NULL, b = 2L),
        "Parameters 'r', 'g', and 'b' must all be supplied together"
    )
    expect_error(
        snic_plot(img, r = 1L, g = 2L, b = 3L),
        "Invalid RGB band index\\. Array has 2 bands\\."
    )
})

test_that("snic_plot.array forwards to terra::plot for grayscale input", {
    skip_if_not_installed("terra")
    img <- array(runif(6), dim = c(2, 3, 1))
    captured <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(img, col = "grey70", main = "demo")
            )
        },
        points = function(...) {
            stop("points should not be called without seeds")
        },
        .package = "graphics"
    ) %>%
        with_mocked_bindings(
            plot = function(x, col, stretch, ...) {
                captured <<- list(
                    x = x,
                    col = col,
                    stretch = stretch,
                    args = list(...)
                )
                invisible(NULL)
            },
            plotRGB = function(...) {
                stop("plotRGB should not be called for single-band arrays")
            },
            .package = "terra"
        )
    expect_true(inherits(captured$x, "SpatRaster"))
    expect_identical(captured$col, "grey70")
    expect_identical(captured$stretch, "lin")
    expect_identical(captured$args$main, "demo")
})

test_that("snic_plot.array uses terra::plotRGB and overlays seeds", {
    skip_if_not_installed("terra")
    img <- array((1:60) / 60, dim = c(5, 4, 3))
    seeds <- .seeds(r = c(1, 3), c = c(1, 4))
    captured <- list()
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(
                    img,
                    r = 1L,
                    g = 2L,
                    b = 3L,
                    seeds = seeds,
                    seeds_plot_args = list(col = "yellow", pch = 9, cex = 2)
                )
            )
        },
        plotRGB = function(x, r, g, b, stretch, ...) {
            captured$rgb <<- list(
                x = x,
                r = r,
                g = g,
                b = b,
                stretch = stretch,
                args = list(...)
            )
            invisible(NULL)
        },
        plot = function(...) {
            stop("plot should not be called for RGB arrays")
        },
        .package = "terra"
    ) %>%
        with_mocked_bindings(
            points = function(x, y, col, pch, cex, ...) {
                captured$points <<- list(
                    x = x,
                    y = y,
                    col = col,
                    pch = pch,
                    cex = cex,
                    extra = list(...)
                )
                invisible(NULL)
            },
            .package = "graphics"
        )
    expect_true(inherits(captured$rgb$x, "SpatRaster"))
    expect_equal(captured$rgb$r, 1L)
    expect_equal(captured$rgb$g, 2L)
    expect_equal(captured$rgb$b, 3L)
    expect_identical(captured$rgb$stretch, "lin")
    expect_equal(captured$points$x, seeds$c)
    expected_y <- terra::nrow(captured$rgb$x) - seeds$r
    expect_equal(captured$points$y, expected_y)
    expect_identical(captured$points$col, "yellow")
    expect_identical(captured$points$pch, 9)
    expect_identical(captured$points$cex, 2)
})

test_that("snic_plot.array validates seeds input", {
    skip_if_not_installed("terra")
    img <- array(runif(12), dim = c(3, 2, 2))
    expect_error(
        snic_plot(img, seeds = matrix(1:3, ncol = 3)),
        "argument 'seeds' must have columns \\(lon, lat\\) or \\(r, c\\)"
    )
    expect_invisible(
        snic_plot(img, seeds = .seeds(r = 0L, c = 1L))
    )
})

test_that("snic_plot.array merges default seed plotting arguments", {
    skip_if_not_installed("terra")
    img <- array((1:6) / 6, dim = c(2, 3, 1))
    seeds <- .seeds(r = c(1, 2), c = c(2, 3))
    captured <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(
                    img,
                    seeds = seeds,
                    seeds_plot_args = list(col = "yellow")
                )
            )
        },
        plot = function(...) {
            invisible(NULL)
        },
        plotRGB = function(...) {
            stop("plotRGB should not be called for single-band arrays")
        },
        .package = "terra"
    ) %>%
        with_mocked_bindings(
            points = function(x, y, col, pch, cex, ...) {
                captured <<- list(
                    x = x,
                    y = y,
                    col = col,
                    pch = pch,
                    cex = cex,
                    extra = list(...)
                )
                invisible(NULL)
            },
            .package = "graphics"
        )
    snic:::rc_to_xy(img, seeds)
    expect_equal(captured$x, seeds$c)
    expect_equal(
        captured$y,
        dim(img)[1] - seeds$r
    )
    expect_identical(captured$col, "yellow")
    expect_identical(captured$pch, 4)
    expect_identical(captured$cex, 1)
})

test_that("snic_plot.default errors for unsupported classes", {
    expect_error(
        snic_plot(list()),
        "Unsupported input type 'list'",
        fixed = TRUE
    )
})

test_that("snic_plot.SpatRaster forwards to terra::plot for single band", {
    skip_if_not_installed("terra")
    r <- terra::rast(
        nrows = 2, ncols = 3, nlyrs = 2,
        xmin = 0, xmax = 3, ymin = 0, ymax = 2
    )
    terra::values(r) <- runif(12)
    captured <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(r, band = 2L, col = "grey50", main = "demo")
            )
        },
        points = function(...) {
            stop("points should not be called without seeds")
        },
        .package = "graphics"
    ) %>%
        with_mocked_bindings(
            plot = function(x, ...) {
                captured <<- list(x = x, args = list(...))
                invisible(NULL)
            },
            plotRGB = function(...) {
                stop("plotRGB should not be called for single-band plots")
            },
            .package = "terra"
        )
    expect_true(inherits(captured$x, "SpatRaster"))
    expect_equal(terra::nlyr(captured$x), 2)
    expect_equal(captured$args$col, "grey50")
    expect_equal(captured$args$main, "demo")
})

test_that("snic_plot.SpatRaster uses plotRGB and overlays seeds", {
    skip_if_not_installed("terra")
    r <- terra::rast(
        nrows = 2, ncols = 2, nlyrs = 3,
        xmin = 0, xmax = 2, ymin = 0, ymax = 2
    )
    terra::values(r) <- runif(12)
    seeds <- .seeds(
        r = c(1, 2),
        c = c(1, 2)
    )
    captured_rgb <- NULL
    captured_points <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(
                    r,
                    r = 1L,
                    g = 2L,
                    b = 3L,
                    seeds = seeds,
                    seeds_plot_args = list(col = "blue", pch = 16)
                )
            )
        },
        points = function(x, y, col, pch, cex, ...) {
            captured_points <<- list(
                x = x,
                y = y,
                col = col,
                pch = pch,
                cex = cex
            )
            invisible(NULL)
        },
        .package = "graphics"
    ) %>%
        with_mocked_bindings(
            plot = function(...) {
                stop("plot should not be called for RGB plots")
            },
            plotRGB = function(x, r, g, b, ...) {
                captured_rgb <<- list(
                    x = x, r = r, g = g, b = b, args = list(...)
                )
                invisible(NULL)
            },
            .package = "terra"
        )
    expect_equal(captured_rgb$r, 1L)
    expect_equal(captured_rgb$g, 2L)
    expect_equal(captured_rgb$b, 3L)
    expected_xy <- terra::xyFromCell(
        r, terra::cellFromRowCol(r, seeds$r, seeds$c)
    )
    expect_equal(captured_points$x, expected_xy[, "x"])
    expect_equal(captured_points$y, expected_xy[, "y"])
    expect_identical(captured_points$col, "blue")
    expect_identical(captured_points$pch, 16)
    expect_identical(captured_points$cex, 1)
})

test_that("snic_plot.SpatRaster plots seeds using row/column indices", {
    skip_if_not_installed("terra")
    r <- terra::rast(
        nrows = 2,
        ncols = 2,
        xmin = 0,
        xmax = 2,
        ymin = 0,
        ymax = 2,
        crs = "EPSG:4326"
    )
    terra::values(r) <- runif(4)
    seeds <- .seeds(
        lat = c(0.5, 1.5),
        lon = c(1.5, 2.5)
    )
    captured_points <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(r, band = 1L, seeds = seeds)
            )
        },
        points = function(x, y, ...) {
            captured_points <<- list(x = x, y = y, extra = list(...))
            invisible(NULL)
        },
        .package = "graphics"
    ) %>%
        with_mocked_bindings(
            plot = function(...) {
                invisible(NULL)
            },
            plotRGB = function(...) {
                stop("plotRGB should not be called for single-band plots")
            },
            .package = "terra"
        )

    expect_equal(captured_points$x, c(1.5, 2.5))
    expect_equal(captured_points$y, c(0.5, 1.5))
})

test_that("snic_plot.SpatRaster overlays segmentation polygons", {
    skip_if_not_installed("terra")
    r <- terra::rast(
        nrows = 2, ncols = 2, nlyrs = 1,
        xmin = 0, xmax = 2, ymin = 0, ymax = 2
    )
    terra::values(r) <- runif(4)
    seg <- terra::rast(r)
    terra::values(seg) <- sample(1:2, terra::ncell(seg), replace = TRUE)
    calls <- list()
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(r, seg = seg, seg_plot_args = list(border = "yellow"))
            )
        },
        plot = function(x, ...) {
            calls[[length(calls) + 1]] <<- list(x = x, args = list(...))
            invisible(NULL)
        },
        plotRGB = function(...) {
            stop("plotRGB should not be called for single-band plots")
        },
        .package = "terra"
    )
    expect_length(calls, 2)
    expect_true(inherits(calls[[1]]$x, "SpatRaster"))
    expect_true(inherits(calls[[2]]$x, "SpatVector"))
    expect_true(isTRUE(calls[[2]]$args$add))
    expect_identical(calls[[2]]$args$border, "yellow")
})
