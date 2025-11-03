test_that("snic.array rejects unused arguments", {
    img <- array(runif(8), dim = c(2, 2, 2))
    seeds <- matrix(c(1L, 1L), ncol = 2L)
    expect_error(
        snic(img, seeds = seeds, compactness = 5, unused = TRUE),
        "Unused arguments for array input"
    )
})

test_that("snic.array requires 3D arrays", {
    img <- array(runif(4), dim = c(2, 2))
    seeds <- matrix(c(1L, 1L), ncol = 2L)
    expect_error(
        snic(img, seeds = seeds, compactness = 1),
        "argument 'img' must have 3 dimensions"
    )
})

test_that("snic.array converts integer data to double", {
    img <- array(as.integer(1:8), dim = c(2, 2, 2))
    reference <- array(as.numeric(1:8), dim = c(2, 2, 2))
    seeds <- matrix(c(1L, 1L), ncol = 2L)
    result_int <- snic(img, seeds = seeds, compactness = 0)
    result_double <- snic(reference, seeds = seeds, compactness = 0)
    expect_equal(dim(result_int)[1:2], c(2L, 2L))
    expect_equal(result_int, result_double)
})

test_that("snic.SpatRaster requires terra namespace", {
    fake_raster <- structure(list(), class = "SpatRaster")
    seeds <- matrix(c(1L, 1L), ncol = 2L)
    expect_error(
        with_mocked_bindings(
            snic(fake_raster, seeds = seeds, compactness = 1),
            requireNamespace = function(pkg, quietly) FALSE,
            .package = "base"
        ),
        "Package 'terra' must be installed"
    )
})

test_that("snic.SpatRaster rejects unused arguments", {
    fake_raster <- structure(list(), class = "SpatRaster")
    seeds <- matrix(c(1L, 1L), ncol = 2L)
    expect_error(
        with_mocked_bindings(
            snic(fake_raster, seeds = seeds, bogus = 1),
            requireNamespace = function(pkg, quietly) TRUE,
            .package = "base"
        ),
        "Unused arguments for SpatRaster input"
    )
})

test_that("snic.SpatRaster errors when values unavailable", {
    skip_if_not_installed("terra")
    fake_raster <- structure(list(), class = "SpatRaster")
    seeds <- matrix(c(1L, 1L), ncol = 2L)
    expect_error(
        with_mocked_bindings(
            with_mocked_bindings(
                snic(fake_raster, seeds = seeds, compactness = 1),
                values = function(...) NULL,
                .package = "terra"
            ),
            requireNamespace = function(pkg, quietly) TRUE,
            .package = "base"
        ),
        "Unable to extract pixel values"
    )
})

test_that("snic.SpatRaster returns raster by default", {
    skip_if_not_installed("terra")

    height <- 3L
    width <- 3L
    img <- terra::rast(nrows = height, ncols = width, nlyrs = 1L)
    terra::values(img) <- runif(height * width)

    seeds <- matrix(c(1L, 1L, 2L, 2L), ncol = 2L, byrow = TRUE)
    result <- snic(img, seeds = seeds, compactness = 5)
    expect_true(inherits(result, "SpatRaster"))
    expect_equal(terra::nrow(result), height)
    expect_equal(terra::ncol(result), width)
})

test_that("snic.default reports unsupported inputs", {
    expect_error(
        snic(list(1, 2, 3), seeds = matrix(c(1L, 1L), ncol = 2L)),
        "no applicable method for 'snic' applied to an object of class \"list\""
    )
})
