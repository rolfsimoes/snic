test_that("rect_grid produces evenly spaced coordinates for arrays", {
    img <- array(runif(144), dim = c(12L, 12L, 1L))
    spacing <- c(3L, 3L)
    padding <- c(1L, 1L)

    seeds <- rect_grid(img, spacing = spacing, padding = padding)
    expect_false(anyNA(seeds))
    expect_equal(ncol(seeds), 2L)
    expect_equal(nrow(seeds), count_seeds(img, spacing, padding))

    rows <- sort(unique(seeds[, 1L]))
    cols <- sort(unique(seeds[, 2L]))
    expect_true(all(diff(rows) == diff(rows)[1L]))
    expect_true(all(diff(cols) == diff(cols)[1L]))
})

test_that("hexagonal_grid yields coordinates within image bounds", {
    img <- array(runif(144), dim = c(12L, 12L, 1L))
    spacing <- 3L
    padding <- 0L

    seeds <- hexagonal_grid(img, spacing = spacing, padding = padding)
    expect_false(anyNA(seeds))
    expect_equal(ncol(seeds), 2L)
    expect_true(all(seeds[, 1L] >= 1 & seeds[, 1L] <= nrow(img)))
    expect_true(all(seeds[, 2L] >= 1 & seeds[, 2L] <= ncol(img)))
})

test_that("random_grid is reproducible and matches seed counts", {
    img <- array(runif(100), dim = c(10L, 10L, 1L))
    spacing <- 3L
    padding <- 1L

    set.seed(42)
    seeds_a <- random_grid(img, spacing = spacing, padding = padding)
    set.seed(42)
    seeds_b <- random_grid(img, spacing = spacing, padding = padding)

    expect_equal(seeds_a, seeds_b)
    expect_equal(nrow(seeds_a), count_seeds(img, spacing, padding))
})

test_that("rect_grid works with SpatRaster input", {
    skip_if_not_installed("terra")

    rast <- terra::rast(nrows = 12, ncols = 12, nlyrs = 1)
    terra::values(rast) <- runif(144)
    spacing <- c(3L, 3L)
    padding <- c(1L, 1L)

    seeds_rast <- rect_grid(rast, spacing = spacing, padding = padding)
    expect_false(anyNA(seeds_rast))
    expect_equal(ncol(seeds_rast), 2L)
    expect_equal(nrow(seeds_rast), count_seeds(rast, spacing, padding))
})

test_that("rect_grid validates spacing inputs", {
    img <- array(0, dim = c(10L, 10L, 1L))

    expect_error(rect_grid(img, spacing = numeric(0)), "cannot be empty", fixed = TRUE)
    expect_error(rect_grid(img, spacing = "a"), "must be numeric", fixed = TRUE)
    expect_error(rect_grid(img, spacing = c(3, NA)), "must contain only finite values", fixed = TRUE)
    expect_error(rect_grid(img, spacing = c(0, 2)), "must be strictly positive", fixed = TRUE)
    expect_error(rect_grid(img, spacing = c(1, 2, 3)), "must have length 1 or 2", fixed = TRUE)
})

test_that("rect_grid validates padding inputs", {
    img <- array(0, dim = c(10L, 10L, 1L))

    expect_error(rect_grid(img, spacing = c(2, 2), padding = "a"), "must be numeric", fixed = TRUE)
    expect_error(rect_grid(img, spacing = c(2, 2), padding = c(1, NA)), "must contain only finite values", fixed = TRUE)
    expect_error(rect_grid(img, spacing = c(2, 2), padding = c(-1, 0)), "must be non-negative", fixed = TRUE)
    expect_error(rect_grid(img, spacing = c(2, 2), padding = c(5, 5)), "leaves no room for seed placement", fixed = TRUE)
    expect_error(rect_grid(img, spacing = c(2, 2), padding = c(1, 2, 3)), "must have length 1 or 2", fixed = TRUE)
})

test_that("rect_grid handles single seed per dimension", {
    img <- array(0, dim = c(10L, 10L, 1L))
    seeds <- rect_grid(img, spacing = c(50, 50), padding = c(1, 1))

    expect_equal(nrow(seeds), 1L)
    expect_equal(as.numeric(seeds[1L, 1L]), mean(c(2, 9)))
    expect_equal(as.numeric(seeds[1L, 2L]), mean(c(2, 9)))
})

test_that("diamond_grid respects asymmetric padding without recycling warnings", {
    img <- array(0, dim = c(50L, 50L, 1L))
    seeds <- diamond_grid(img, spacing = c(8, 8), padding = c(1, 20))

    expect_false(anyNA(seeds))
    expect_true(all(seeds[, 1L] >= 1 & seeds[, 1L] <= nrow(img)))
    expect_true(all(seeds[, 2L] >= 1 & seeds[, 2L] <= ncol(img)))
})

test_that("diamond_grid filters use axis-specific padding", {
    img <- array(0, dim = c(40L, 60L, 1L))
    spacing <- c(7, 9)
    padding <- c(2, 18)

    expect_no_warning({
        seeds <- diamond_grid(img, spacing = spacing, padding = padding)
    })

    expect_gt(nrow(seeds), 0L)
    expect_true(all(seeds[, 1L] <= nrow(img) - padding[[1]]))
    expect_true(all(seeds[, 2L] <= ncol(img) - padding[[2]]))
})

test_that("hexagonal_grid filters use axis-specific padding", {
    img <- array(0, dim = c(45L, 70L, 1L))
    spacing <- c(6, 5)
    padding <- c(3, 12)

    expect_no_warning({
        seeds <- hexagonal_grid(img, spacing = spacing, padding = padding)
    })

    expect_gt(nrow(seeds), 0L)
    expect_true(all(seeds[, 1L] <= nrow(img) - padding[[1]]))
    expect_true(all(seeds[, 2L] <= ncol(img) - padding[[2]]))
})

test_that("random_grid validates inputs and interior area", {
    img <- array(0, dim = c(10L, 10L, 1L))

    expect_error(random_grid(img, spacing = numeric(0)), "cannot be empty", fixed = TRUE)
    expect_error(
        random_grid(img, spacing = c(20, 20), padding = c(4.9, 4.9)),
        "yields no valid seed positions",
        fixed = TRUE
    )
    expect_error(
        random_grid(img, spacing = c(0.2, 0.2), padding = c(0, 0)),
        "Requested seed count exceeds available area",
        fixed = TRUE
    )
})
