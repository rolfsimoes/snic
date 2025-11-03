test_that("snic validates matrix-based inputs", {
    img <- array(runif(4), dim = c(2L, 2L, 1L))

    expect_error(snic(img), "argument \"seeds\" is missing, with no default")

    seeds_valid <- matrix(c(1L, 1L), ncol = 2L)
    expect_error(
        snic(img, seeds = seeds_valid, compactness = -1),
        "argument 'compactness' must be a non-negative finite number"
    )

    expect_error(
        snic(array(rep("a", 4L), dim = c(4L, 1L, 1L)), seeds = seeds_valid),
        "argument 'img' must be a numeric array"
    )

    seeds <- matrix(c(0L, 1L), ncol = 2L, byrow = TRUE)
    expect_error(
        snic(img, seeds = seeds),
        "argument 'seeds' coordinates must lie within image bounds"
    )

    result <- snic(img, seeds = seeds_valid, compactness = 0)
    expect_true(is.array(result))
    expect_equal(dim(result), c(2L, 2L, 1L))
})

test_that("rect_grid and count_seeds provide consistent coordinates", {
    img <- array(runif(16), dim = c(4L, 4L, 1L))
    spacing <- c(2L, 2L)
    padding <- c(0L, 0L)

    seeds <- rect_grid(img, spacing = spacing, padding = padding)
    seeds <- round(seeds)
    storage.mode(seeds) <- "integer"

    expect_true(is.matrix(seeds))
    expect_equal(ncol(seeds), 2L)
    expect_equal(nrow(seeds), count_seeds(img, spacing, padding))
    expect_true(all(seeds[, 1L] >= 1L & seeds[, 1L] <= nrow(img)))
    expect_true(all(seeds[, 2L] >= 1L & seeds[, 2L] <= ncol(img)))
})
