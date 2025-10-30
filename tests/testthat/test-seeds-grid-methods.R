test_that("snic_seeds_grid.array rejects unused arguments", {
    img <- array(runif(8), dim = c(2, 2, 2))
    expect_error(
        snic_seeds_grid(img, grid_step = 1L, unused = TRUE),
        "Unused arguments for array input"
    )
})

test_that("snic_seeds_grid.array requires 3D arrays", {
    img <- array(runif(4), dim = c(2, 2))
    expect_error(
        snic_seeds_grid(img, grid_step = 1L),
        "must have 3 \\(height, width, bands\\) dimensions"
    )
})

test_that("snic_seeds_grid.array accepts non-integer grid steps", {
    img <- array(runif(27), dim = c(3, 3, 3))
    res_int <- snic_seeds_grid(img, grid_step = 2L)
    res_double <- snic_seeds_grid(img, grid_step = 2)
    expect_identical(res_int, res_double)
})

test_that("snic_seeds_grid.SpatRaster requires terra namespace", {
    fake_raster <- structure(list(), class = "SpatRaster")
    expect_error(
        with_mocked_bindings(
            snic_seeds_grid(fake_raster, grid_step = 1L),
            requireNamespace = function(pkg, quietly) FALSE,
            .package = "base"
        ),
        "Package 'terra' must be installed"
    )
})

test_that("snic_seeds_grid.SpatRaster rejects unused arguments", {
    fake_raster <- structure(list(), class = "SpatRaster")
    expect_error(
        with_mocked_bindings(
            snic_seeds_grid(fake_raster, grid_step = 1L, unused = TRUE),
            requireNamespace = function(pkg, quietly) TRUE,
            .package = "base"
        ),
        "Unused arguments for SpatRaster input"
    )
})

test_that("snic_seeds_grid.SpatRaster errors when values unavailable", {
    skip_if_not_installed("terra")
    rast <- terra::rast(nrows = 2, ncols = 2, nlyrs = 1)
    expect_error(
        with_mocked_bindings(
            snic_seeds_grid(rast, grid_step = 1L),
            values = function(...) NULL,
            .package = "terra"
        ),
        "Unable to extract pixel values"
    )
})

test_that("snic_seeds_grid.SpatRaster accepts non-integer grid steps", {
    skip_if_not_installed("terra")
    rast <- terra::rast(nrows = 4, ncols = 4, nlyrs = 1)
    terra::values(rast) <- runif(16)
    res_int <- snic_seeds_grid(rast, grid_step = 2L)
    res_double <- snic_seeds_grid(rast, grid_step = 2)
    expect_identical(res_int, res_double)
})

test_that("snic_seeds_grid.default reports unsupported inputs", {
    expect_error(
        snic_seeds_grid(list(1, 2, 3)),
        "Unsupported input type"
    )
})
