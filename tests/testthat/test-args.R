test_that("snic validates matrix-based inputs", {
    img <- array(runif(4), dim = c(2L, 2L, 1L))

    expect_error(
        snic(img, grid_step = 0L),
        "Argument `grid_step` must be a positive integer"
    )

    expect_error(
        snic(img, grid_step = 1L, compactness = -1),
        "Argument `compactness` must be a non-negative finite number"
    )

    expect_error(
        snic(array(rep("a", 4L), dim = c(4L, 1L, 1L)), grid_step = 1L),
        "Argument `img` must be a numeric array"
    )

    seeds <- matrix(c(0L, 1L), ncol = 2L, byrow = TRUE)
    expect_error(
        snic(img, seeds = seeds),
        "Argument `seeds` coordinates must lie within image bounds"
    )

    result <- snic(img, grid_step = 1L, compactness = 0)
    expect_true(is.array(result))
    expect_equal(dim(result), c(2L, 2L, 1L))
})

test_that("snic_seeds_grid enforces argument contracts", {
    img <- array(runif(4), dim = c(2L, 2L, 1L))

    expect_error(
        snic_seeds_grid(
            array(rep("a", 4), dim = c(2L, 2L, 1L)),
            grid_step = 1L
        ),
        "Argument `img` must be a numeric array"
    )

    expect_error(
        snic_seeds_grid(img, grid_step = 0L),
        "Argument `grid_step` must be a positive integer"
    )

    seeds <- snic_seeds_grid(img, grid_step = 1L)
    expect_true(is.matrix(seeds))
    expect_equal(dim(seeds)[2], 2L)
})
