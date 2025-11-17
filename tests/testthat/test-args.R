test_that("snic validates seed inputs", {
    img <- array(runif(4), dim = c(2L, 2L, 1L))

    expect_error(snic(img), "argument \"seeds\" is missing, with no default")

    seeds_valid <- data.frame(r = 1L, c = 1L)
    expect_error(
        snic(img, seeds = seeds_valid, compactness = -1),
        "argument 'compactness' must be a non-negative finite number"
    )

    expect_error(
        snic(array(rep("a", 4L), dim = c(4L, 1L, 1L)), seeds = seeds_valid),
        "argument 'x' must be numeric"
    )

    seeds <- data.frame(r = 0L, c = 1L)
    expect_error(
        snic(img, seeds = seeds),
        "argument 'seeds' coordinates must lie within image bounds"
    )

    result <- snic(img, seeds = seeds_valid, compactness = 0)
    expect_true(is.list(result))
    expect_equal(dim(snic_get_seg(result)), c(2L, 2L, 1L))
})

test_that("snic_grid and snic_count_seeds provide consistent results", {
    img <- array(runif(16), dim = c(4L, 4L, 1L))
    spacing <- c(2L, 2L)
    padding <- c(0L, 0L)

    seeds <- snic_grid(
        img,
        type = "rectangular",
        spacing = spacing,
        padding = padding
    )
    expect_true(is.data.frame(seeds))
    expect_identical(colnames(seeds), c("r", "c"))
    expect_equal(nrow(seeds), snic_count_seeds(img, spacing, padding))
    expect_true(all(seeds$r >= 1L & seeds$r <= nrow(img)))
    expect_true(all(seeds$c >= 1L & seeds$c <= ncol(img)))
})
