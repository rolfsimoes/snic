test_that("snic validates matrix-based inputs", {
    img <- matrix(runif(4), nrow = 4, ncol = 1)

    expect_error(
        snic(img, width = 0, height = 2),
        "Argument 'width' must be a positive finite numeric scalar"
    )

    expect_error(
        snic(img, width = 2, height = 0),
        "Argument 'height' must be a positive finite numeric scalar"
    )

    expect_error(
        snic(img, width = 2, height = 2, grid_step = 0),
        "Argument 'grid_step' must be a positive finite numeric scalar"
    )

    expect_error(
        snic(img, width = 2, height = 2, compactness = -1),
        "Argument 'compactness' must be a non-negative finite numeric scalar"
    )

    expect_error(
        snic(matrix("a", nrow = 4, ncol = 1), width = 2, height = 2),
        "Argument 'img' must be a numeric matrix"
    )

    seeds <- matrix(c(0L, 1L), ncol = 2L, byrow = TRUE)
    expect_error(
        snic(img, width = 2, height = 2, seeds = seeds),
        "Argument 'seeds' coordinates must lie within image bounds"
    )

    result <- snic(img, width = 2, height = 2, grid_step = 1, compactness = 0)
    expect_true(is.matrix(result))
    expect_equal(dim(result), c(2L, 2L))
})

test_that("snic_seeds_grid enforces argument contracts", {
    img <- matrix(runif(4), nrow = 4, ncol = 1)

    expect_error(
        snic_seeds_grid(matrix("a", nrow = 4, ncol = 1), width = 2, height = 2, step = 1),
        "Argument 'img' must be a numeric matrix"
    )

    expect_error(
        snic_seeds_grid(img, width = 0, height = 2, step = 1),
        "Argument 'width' must be a positive finite numeric scalar"
    )

    expect_error(
        snic_seeds_grid(img, width = 2, height = 0, step = 1),
        "Argument 'height' must be a positive finite numeric scalar"
    )

    expect_error(
        snic_seeds_grid(img, width = 2, height = 2, step = 0),
        "Argument 'step' must be a positive finite numeric scalar"
    )

    expect_error(
        snic_seeds_grid(img, width = 3, height = 2, step = 1),
        "width \\* height must equal the number of rows in img"
    )

    seeds <- snic_seeds_grid(img, width = 2, height = 2, step = 1)
    expect_true(is.matrix(seeds))
    expect_equal(dim(seeds)[2], 2L)
})
