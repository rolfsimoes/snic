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
            {
                snic(fake_raster, seeds = seeds, bogus = 1)
            },
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
            {
                snic(fake_raster, seeds = seeds, compactness = 1)
            },
            values = function(...) NULL,
            .package = "terra"
        ) %>%
            with_mocked_bindings(
                requireNamespace = function(pkg, quietly) TRUE,
                .package = "base"
            ),
        "SpatRaster must define a CRS to compute geographic"
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

test_that("snic.array ignores lat/lon seed metadata", {
    height <- 4L
    width <- 4L
    img <- array(runif(height * width), dim = c(height, width, 1L))
    seeds <- snic_grid_rect(
        img,
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    seeds$lat <- seq_len(nrow(seeds))
    seeds$lon <- seq_len(nrow(seeds)) * 2
    original_snic <- snic:::.snic
    captured <- NULL
    with_mocked_bindings(
        {
            snic(img, seeds = seeds, compactness = 1)
        },
        .snic = function(img_data, seeds, compactness, order) {
            captured <<- seeds
            original_snic(img_data, seeds, compactness, order)
        },
        .package = "snic"
    )
    expect_identical(ncol(captured), 4L)
    expect_equal(colnames(captured), c("r", "c", "lat", "lon"))
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

    seeds <- snic_grid_rect(
        img,
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    seeds_misaligned <- seeds
    seeds_misaligned$r <- 1L
    seeds_misaligned$c <- 1L

    original_snic <- snic:::.snic
    captured <- NULL
    with_mocked_bindings(
        {
            snic(img, seeds = seeds_misaligned, compactness = 1)
        },
        .snic = function(img_data, seeds, compactness, order) {
            captured <<- seeds
            original_snic(img_data, seeds, compactness, order)
        },
        .package = "snic"
    )

    expect_false(all(captured[, 1] == 1L & captured[, 2] == 1L))
    expect_equal(captured[, 1], seeds$r)
    expect_equal(captured[, 2], seeds$c)
})
