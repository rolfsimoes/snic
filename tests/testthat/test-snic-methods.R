test_that("snic.array rejects unused arguments", {
    img <- array(runif(8), dim = c(2, 2, 2))
    expect_error(
        snic(img, grid_step = 1L, compactness = 5, unused = TRUE),
        "Unused arguments for array input"
    )
})

test_that("snic.array requires 3D arrays", {
    img <- array(runif(4), dim = c(2, 2))
    expect_error(
        snic(img, grid_step = 1L, compactness = 1),
        "must have 3 \\(height, width, bands\\) dimensions"
    )
})

test_that("snic.array converts integer data to double", {
    img <- array(as.integer(1:8), dim = c(2, 2, 2))
    reference <- array(as.numeric(1:8), dim = c(2, 2, 2))
    result_int <- snic(img, grid_step = 1L, compactness = 0)
    result_double <- snic(reference, grid_step = 1L, compactness = 0)
    expect_equal(dim(result_int)[1:2], c(2L, 2L))
    expect_equal(result_int, result_double)
})

test_that("snic.SpatRaster requires terra namespace", {
    fake_raster <- structure(list(), class = "SpatRaster")
    expect_error(
        with_mocked_bindings(
            snic(fake_raster, grid_step = 1L, compactness = 1),
            requireNamespace = function(pkg, quietly) FALSE,
            .package = "base"
        ),
        "Package 'terra' must be installed"
    )
})

test_that("snic.SpatRaster rejects unused arguments", {
    fake_raster <- structure(list(), class = "SpatRaster")
    expect_error(
        with_mocked_bindings(
            snic(fake_raster, bogus = 1),
            requireNamespace = function(pkg, quietly) TRUE,
            .package = "base"
        ),
        "Unused arguments for SpatRaster input"
    )
})

test_that("snic.SpatRaster errors when values unavailable", {
    skip_if_not_installed("terra")
    fake_raster <- structure(list(), class = "SpatRaster")
    expect_error(
        with_mocked_bindings(with_mocked_bindings(
            snic(fake_raster, grid_step = 1L, compactness = 1),
            values = function(...) NULL,
            .package = "terra"
        ),
        requireNamespace = function(pkg, quietly) TRUE,
        .package = "base"),
        "Unable to extract pixel values"
    )
})

test_that("snic.SpatRaster returns raster when option set", {
    skip_if_not_installed("terra")
    old_opt <- getOption("snic.return_raster")
    on.exit(options(snic.return_raster = old_opt), add = TRUE)
    options(snic.return_raster = TRUE)

    height <- 3L
    width <- 3L
    img <- terra::rast(nrows = height, ncols = width, nlyrs = 1L)
    terra::values(img) <- runif(height * width)

    result <- snic(img, grid_step = 1L, compactness = 5)
    expect_true(inherits(result, "SpatRaster"))
    expect_equal(terra::nrow(result), height)
    expect_equal(terra::ncol(result), width)
})

test_that("snic.default reports unsupported inputs", {
    expect_error(
        snic(list(1, 2, 3)),
        "Unsupported input type"
    )
})
