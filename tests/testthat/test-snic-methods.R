test_that("snic.array ignores unused arguments", {
    img <- array(runif(8), dim = c(2, 2, 2))
    seeds <- data.frame(r = 1L, c = 1L)
    expect_no_error(
        snic(img, seeds = seeds, compactness = 5, unused = TRUE)
    )
})

test_that("snic.array requires 3D arrays", {
    img <- array(runif(4), dim = c(2, 2))
    seeds <- data.frame(r = 1L, c = 1L)
    expect_error(
        snic(img, seeds = seeds, compactness = 1),
        "argument 'img' must be a numeric array with three dimensions",
        fixed = TRUE
    )
})

test_that("snic.array converts integer data to double", {
    img <- array(as.integer(1:8), dim = c(2, 2, 2))
    reference <- array(as.numeric(1:8), dim = c(2, 2, 2))
    seeds <- data.frame(r = 1L, c = 1L)
    result_int <- snic(img, seeds = seeds, compactness = 0)
    result_double <- snic(reference, seeds = seeds, compactness = 0)
    expect_equal(dim(result_int)[1:2], c(2L, 2L))
    expect_equal(result_int, result_double)
})

test_that("snic.SpatRaster ignores unused arguments", {
    skip_if_not_installed("terra")
    img <- terra::rast(nrows = 2, ncols = 2, nlyrs = 1)
    terra::values(img) <- runif(4)
    seeds <- data.frame(r = 1L, c = 1L)
    expect_no_error(
        snic(img, seeds = seeds, bogus = 1)
    )
})

test_that("snic.SpatRaster returns raster by default", {
    skip_if_not_installed("terra")

    height <- 3L
    width <- 3L
    img <- terra::rast(nrows = height, ncols = width, nlyrs = 1L)
    terra::values(img) <- runif(height * width)

    seeds <- data.frame(
        r = c(1L, 2L),
        c = c(1L, 2L)
    )
    result <- snic(img, seeds = seeds, compactness = 5)
    expect_true(inherits(result, "SpatRaster"))
    expect_equal(terra::nrow(result), height)
    expect_equal(terra::ncol(result), width)
})

test_that("snic.default reports unsupported inputs", {
    expect_error(
        snic(list(1, 2, 3), seeds = data.frame(r = 1L, c = 1L)),
        "Unsupported input type 'list'",
        fixed = TRUE
    )
})

test_that("snic.array ignores lat/lon seed metadata", {
    height <- 4L
    width <- 4L
    img <- array(runif(height * width), dim = c(height, width, 1L))
    seeds <- snic_grid(
        img,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    seeds$lat <- seq_len(nrow(seeds))
    seeds$lon <- seq_len(nrow(seeds)) * 2
    captured <- NULL
    original_call <- snic:::.call
    with_mocked_bindings(
        {
            snic(img, seeds = seeds, compactness = 1)
        },
        .call = function(fn_name, ...) {
            if (identical(fn_name, "snic_snic")) {
                args <- list(...)
                captured <<- args[[2L]]
                return(array(1L, dim = c(dim(args[[1L]])[1], dim(args[[1L]])[2], 1L)))
            }
            original_call(fn_name, ...)
        },
        .package = "snic"
    )
    expect_identical(ncol(captured), ncol(seeds))
    expect_equal(colnames(captured), colnames(seeds))
    expect_true(all(captured[, 1] >= 1L & captured[, 1] <= height))
    expect_true(all(captured[, 2] >= 1L & captured[, 2] <= width))
})

test_that("snic.SpatRaster recomputes indices from lat/lon metadata", {
    skip_if_not_installed("terra")
    img <- terra::rast(
        nrows = 4,
        ncols = 4,
        xmin = 0,
        xmax = 4,
        ymin = 0,
        ymax = 4,
        crs = "EPSG:4326"
    )
    terra::values(img) <- runif(16)

    seeds <- snic_grid(
        img,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    seeds_latlon <- seeds[, c("lat", "lon")]

    captured <- NULL
    original_call <- snic:::.call
    with_mocked_bindings(
        {
            snic(img, seeds = seeds_latlon, compactness = 1)
        },
        .call = function(fn_name, ...) {
            if (identical(fn_name, "snic_snic")) {
                args <- list(...)
                captured <<- args[[2L]]
                return(array(
                    1L,
                    dim = c(dim(args[[1L]])[1], dim(args[[1L]])[2], 1L)
                ))
            }
            original_call(fn_name, ...)
        },
        .package = "snic"
    )

    expect_equal(captured[, 1], seeds$r)
    expect_equal(captured[, 2], seeds$c)
})
