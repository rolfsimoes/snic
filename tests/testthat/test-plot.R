test_that("snic_plot.array requires 3D numeric arrays", {
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

test_that("snic_plot.array draws grayscale images with defaults", {
    img <- array(runif(6), dim = c(2, 3, 1))
    captured <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                snic_plot(img, col = "grey70", main = "demo")
            )
        },
        image = function(...) {
            captured <<- list(...)
            invisible(NULL)
        },
        rasterImage = function(...) {
            stop("rasterImage should not be called for grayscale inputs")
        },
        points = function(...) {
            stop("points should not be called without seeds")
        },
        .package = "graphics"
    )
    expect_equal(captured$x, 0:dim(img)[2])
    expect_equal(captured$y, 0:dim(img)[1])
    expect_equal(captured$col, "grey70")
    expect_equal(captured$main, "demo")
    expect_equal(captured$asp, 1)
    expect_equal(captured$ylim, c(dim(img)[1], 0))
})

test_that("snic_plot.array builds RGB composites and overlays seeds", {
    img <- array((1:60) / 60, dim = c(5, 4, 3))
    seeds <- .seeds(
        r = c(1, 3),
        c = c(1, 4)
    )
    captured <- list()
    stretch_calls <- list()
    stretch_returns <- list(
        matrix(0.1, nrow = dim(img)[1], ncol = dim(img)[2]),
        matrix(0.2, nrow = dim(img)[1], ncol = dim(img)[2]),
        matrix(0.3, nrow = dim(img)[1], ncol = dim(img)[2])
    )
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
        plot.new = function() {
            captured$plot_new <<- TRUE
            invisible(NULL)
        },
        plot.window = function(...) {
            captured$window <<- list(...)
            invisible(NULL)
        },
        rasterImage = function(image, xleft, ybottom, xright, ytop, ...) {
            captured$raster <<- list(
                image = image,
                xleft = xleft,
                ybottom = ybottom,
                xright = xright,
                ytop = ytop,
                args = list(...)
            )
            invisible(NULL)
        },
        image = function(...) {
            stop("image should not be called for RGB inputs")
        },
        par = function(...) {
            invisible(NULL)
        },
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
    ) %>%
        with_mocked_bindings(
            .stretch_band = function(x, stretch, ...) {
                idx <- length(stretch_calls) + 1L
                stretch_calls[[idx]] <<- list(
                    x = x,
                    stretch = stretch,
                    extra = list(...)
                )
                if (idx > length(stretch_returns)) {
                    stop("Unexpected .stretch_band invocation count")
                }
                stretch_returns[[idx]]
            },
            .package = "snic"
        )
    expect_true(isTRUE(captured$plot_new))
    expect_equal(captured$window$xlim, c(0, dim(img)[2]))
    expect_equal(captured$window$ylim, c(0, dim(img)[1]))
    expect_identical(captured$window$asp, 1)
    expect_equal(dim(captured$raster$image), c(dim(img)[1], dim(img)[2], 4L))
    expect_equal(captured$raster$xleft, 0)
    expect_equal(captured$raster$ybottom, 0)
    expect_equal(captured$raster$xright, dim(img)[2])
    expect_equal(captured$raster$ytop, dim(img)[1])
    expect_identical(captured$raster$args$interpolate, FALSE)
    expect_equal(length(stretch_calls), 3)
    expect_equal(stretch_calls[[1]]$x, img[, , 1])
    expect_equal(stretch_calls[[2]]$x, img[, , 2])
    expect_equal(stretch_calls[[3]]$x, img[, , 3])
    expect_identical(stretch_calls[[1]]$stretch, "lin")
    expect_identical(stretch_calls[[2]]$stretch, "lin")
    expect_identical(stretch_calls[[3]]$stretch, "lin")
    expect_identical(stretch_calls[[1]]$extra, list())
    expect_identical(captured$raster$image[, , 1], stretch_returns[[1]])
    expect_identical(captured$raster$image[, , 2], stretch_returns[[2]])
    expect_identical(captured$raster$image[, , 3], stretch_returns[[3]])
    expect_identical(
        captured$raster$image[, , 4],
        matrix(1, nrow = dim(img)[1], ncol = dim(img)[2])
    )
    expect_equal(captured$points$x, seeds$c)
    expect_equal(captured$points$y, seeds$r)
    expect_identical(captured$points$col, "yellow")
    expect_identical(captured$points$pch, 9)
    expect_identical(captured$points$cex, 2)
    expect_identical(captured$points$extra, list())
})

test_that("snic_plot.array validates seeds input", {
    img <- array(runif(12), dim = c(3, 2, 2))
    expect_error(
        snic_plot(img, seeds = matrix(1:3, ncol = 3)),
        "argument 'seeds' must have exactly two columns",
        fixed = TRUE
    )
    expect_invisible(
        snic_plot(img, seeds = matrix(c(0, 1), ncol = 2))
    )
})

test_that("snic_plot.array merges default seeds plotting arguments", {
    img <- array((1:6) / 6, dim = c(2, 3, 1))
    seeds <- .seeds(
        r = c(1, 2),
        c = c(2, 3)
    )
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
        image = function(...) {
            invisible(NULL)
        },
        rasterImage = function(...) {
            stop("rasterImage should not be called for grayscale inputs")
        },
        plot.new = function() {
            invisible(NULL)
        },
        plot.window = function(...) {
            invisible(NULL)
        },
        par = function(...) {
            invisible(NULL)
        },
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
    expect_equal(captured$x, seeds$c)
    expect_equal(captured$y, seeds$r)
    expect_identical(captured$col, "yellow")
    expect_identical(captured$pch, 4)
    expect_identical(captured$cex, 1)
    expect_identical(captured$extra, list())
})

test_that("snic_plot.default errors for unsupported classes", {
    expect_error(
        snic_plot(list()),
        paste(
            "no applicable method for 'snic_plot' applied",
            "to an object of class \"list\""
        )
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
    expect_equal(terra::nlyr(captured$x), 1)
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
                snic_plot(r,
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
    expect_equal(captured_points$x, expected_xy[, 1])
    expect_equal(captured_points$y, expected_xy[, 2])
    expect_identical(captured_points$col, "blue")
    expect_identical(captured_points$pch, 16)
    expect_identical(captured_points$cex, 1)
})

test_that("snic_plot.SpatRaster prioritizes lat/lon metadata when available", {
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
        r = c(99L, 199L),
        c = c(15L, 115L),
        lat = c(0.5, 1.5),
        lon = c(1.5, 2.5),
        y = c(0.5, 1.5),
        x = c(1.5, 2.5)
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
    expect_equal(captured_points$x, seeds$lon[[1]])
    expect_equal(captured_points$y, seeds$lat[[1]])
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
