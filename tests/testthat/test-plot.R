plot_array <- function(...) {
    getS3method("plot", "array", "snic")(...)
}

test_that("plot.array requires 3D arrays", {
    img <- array(runif(4), dim = c(2, 2))
    expect_error(
        plot_array(img),
        "`x` must be a 3D array with dimensions \\(height, width, bands\\)"
    )
})

test_that("plot.array validates band indices", {
    img <- array(runif(8), dim = c(2, 2, 2))
    expect_error(
        plot_array(img, band = 3L),
        "Invalid `band` index \\(3\\). Array has 2 bands\\."
    )
})

test_that("plot.array forwards arguments to plot_snic_matrix", {
    img <- array(runif(12), dim = c(2, 3, 2))
    seeds <- matrix(c(1, 1, 2, 3), ncol = 2, byrow = TRUE)
    captured <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                plot_array(img,
                    seeds = seeds,
                    band = 2L,
                    col = "purple",
                    seed_col = "green",
                    seed_pch = 8,
                    seed_cex = 1.5,
                    main = "demo"
                )
            )
        },
        plot_snic_matrix = function(...) {
            captured <<- list(...)
            invisible(NULL)
        },
        .package = "snic"
    )
    expect_equal(captured$img, img[, , 2L, drop = FALSE])
    expect_equal(captured$seeds, seeds)
    expect_identical(captured$band, 2L)
    expect_identical(captured$col, "purple")
    expect_identical(captured$seed_col, "green")
    expect_identical(captured$seed_pch, 8)
    expect_identical(captured$seed_cex, 1.5)
    expect_identical(captured$image_args$main, "demo")
})

test_that("plot_snic_matrix requires 3D numeric arrays", {
    img <- array(runif(4), dim = c(2, 2))
    expect_error(
        snic:::plot_snic_matrix(img),
        "`img` must be a 3D array with dimensions \\(height, width, bands\\)"
    )
})

test_that("plot_snic_matrix validates band selection", {
    img <- array(runif(12), dim = c(2, 2, 3))
    expect_error(
        snic:::plot_snic_matrix(img, band = 4L),
        "Invalid band index \\(max = 3\\)"
    )
})

test_that("plot_snic_matrix draws grayscale images with default arguments", {
    img <- array(runif(6), dim = c(2, 3, 1))
    captured <- NULL
    with_mocked_bindings(
        {
            expect_invisible(snic:::plot_snic_matrix(img, col = "grey70"))
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
    expect_equal(captured$x, 0.5 + 0:dim(img)[2])
    expect_equal(captured$y, 0.5 + 0:dim(img)[1])
    expect_equal(captured$col, "grey70")
    expect_equal(captured$asp, 1)
    expect_equal(captured$ylim, c(dim(img)[1] + 0.5, 0.5))
})

test_that("plot_snic_matrix builds RGB composites and overlays seeds", {
    img <- array(runif(24), dim = c(3, 4, 3))
    seeds <- matrix(c(1, 1, 3, 4), ncol = 2, byrow = TRUE)
    captured_raster <- NULL
    captured_points <- NULL
    with_mocked_bindings(
        {
            expect_invisible(
                snic:::plot_snic_matrix(
                    img,
                    band = NULL,
                    seeds = seeds,
                    seed_col = "yellow",
                    seed_pch = 9,
                    seed_cex = 2
                )
            )
        },
        rasterImage = function(image, xleft, ybottom, xright, ytop, ...) {
            captured_raster <<- list(
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
        points = function(x, y, pch, col, cex, ...) {
            captured_points <<- list(
                x = x,
                y = y,
                pch = pch,
                col = col,
                cex = cex
            )
            invisible(NULL)
        },
        .package = "graphics"
    )
    expect_equal(dim(captured_raster$image), c(dim(img)[2], dim(img)[1], 3))
    expect_equal(captured_raster$xleft, 1)
    expect_equal(captured_raster$ybottom, 1)
    expect_equal(captured_raster$xright, dim(img)[2])
    expect_equal(captured_raster$ytop, dim(img)[1])
    expect_identical(captured_raster$args$interpolate, FALSE)

    expect_equal(captured_points$x, seeds[, 2])
    expect_equal(captured_points$y, seeds[, 1])
    expect_identical(captured_points$pch, 9)
    expect_identical(captured_points$col, "yellow")
    expect_identical(captured_points$cex, 2)
})

test_that("plot_snic_matrix validates seeds input", {
    img <- array(runif(12), dim = c(3, 2, 2))
    expect_error(
        snic:::plot_snic_matrix(img, seeds = matrix(1:3, ncol = 3)),
        "`seeds` must be a matrix with two columns \\(row, column\\)"
    )
    expect_error(
        snic:::plot_snic_matrix(img, seeds = matrix(c(1, 1.5), ncol = 2)),
        "`seeds` must contain integer coordinates"
    )
    expect_error(
        snic:::plot_snic_matrix(img, seeds = matrix(integer(0), ncol = 2)),
        "`seeds` must contain at least one coordinate"
    )
    expect_error(
        snic:::plot_snic_matrix(img, seeds = matrix(c(0, 1), ncol = 2)),
        "`seeds` must lie within the image bounds"
    )
})
