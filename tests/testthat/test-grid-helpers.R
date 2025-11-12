test_that("snic_grid() rectangular layout produces evenly spaced coordinates for arrays", {
    img <- array(runif(144), dim = c(12L, 12L, 1L))
    spacing <- c(3L, 3L)
    padding <- c(1L, 1L)

    seeds <- snic_grid(
        img,
        type = "rectangular",
        spacing = spacing,
        padding = padding
    )
    expect_false(anyNA(seeds))
    expect_true(is.data.frame(seeds))
    expect_identical(colnames(seeds), c("r", "c"))
    expect_equal(nrow(seeds), snic_count_seeds(img, spacing, padding))

    rows <- sort(unique(seeds$r))
    cols <- sort(unique(seeds$c))
    expect_true(all(diff(rows) == diff(rows)[1L]))
    expect_true(all(diff(cols) == diff(cols)[1L]))
})

test_that("snic_grid(type = \"hexagonal\") yields coordinates within image bounds", {
    img <- array(runif(144), dim = c(12L, 12L, 1L))
    spacing <- 3L
    padding <- 0L

    seeds <- snic_grid(
        img,
        type = "hexagonal",
        spacing = spacing,
        padding = padding
    )
    expect_false(anyNA(seeds))
    expect_true(is.data.frame(seeds))
    expect_identical(colnames(seeds), c("r", "c"))
    expect_true(all(seeds$r >= 1 & seeds$r <= nrow(img)))
    expect_true(all(seeds$c >= 1 & seeds$c <= ncol(img)))
})

test_that("snic_grid(type = \"random\") is reproducible and matches seed counts", {
    img <- array(runif(100), dim = c(10L, 10L, 1L))
    spacing <- 3L
    padding <- 1L

    set.seed(42)
    seeds_a <- snic_grid(
        img,
        type = "random",
        spacing = spacing,
        padding = padding
    )
    set.seed(42)
    seeds_b <- snic_grid(
        img,
        type = "random",
        spacing = spacing,
        padding = padding
    )

    expect_equal(seeds_a, seeds_b)
    expect_equal(nrow(seeds_a), snic_count_seeds(img, spacing, padding))
})

test_that("snic_grid() rectangular layout works with SpatRaster input", {
    skip_if_not_installed("terra")

    rast <- terra::rast(nrows = 12, ncols = 12, nlyrs = 1)
    terra::values(rast) <- runif(144)
    spacing <- c(3L, 3L)
    padding <- c(1L, 1L)

    seeds_rast <- snic_grid(
        rast,
        type = "rectangular",
        spacing = spacing,
        padding = padding
    )
    expect_false(anyNA(seeds_rast))
    expect_true(is.data.frame(seeds_rast))
    expect_identical(colnames(seeds_rast), c("r", "c", "lat", "lon", "y", "x"))
    expect_equal(nrow(seeds_rast), snic_count_seeds(rast, spacing, padding))
    expect_true(all(is.finite(seeds_rast$lat)))
    expect_true(all(is.finite(seeds_rast$lon)))
    expect_true(all(abs(seeds_rast$lat) <= 90))
    expect_true(all(abs(seeds_rast$lon) <= 180))
})

test_that("snic_grid() validates rectangular spacing inputs", {
    img <- array(0, dim = c(10L, 10L, 1L))

    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = numeric(0)
        ),
        "must have length 1 or 2",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = "a",
            padding = c(0, 0)
        ),
        "must be numeric",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(3, NA)
        ),
        "must contain only finite values",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(0, 2)
        ),
        "argument 'spacing' must be greater than 1",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(2, 2, 3)
        ),
        "must have length 1 or 2",
        fixed = TRUE
    )
})

test_that("snic_grid() validates rectangular padding inputs", {
    img <- array(0, dim = c(10L, 10L, 1L))

    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(2, 2),
            padding = "a"
        ),
        "must be numeric",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(2, 2),
            padding = c(1, NA)
        ),
        "must contain only finite values",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(2, 2),
            padding = c(-1, 0)
        ),
        "must be non-negative",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(2, 2),
            padding = c(5, 5)
        ),
        "leaves no room for seed placement",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "rectangular",
            spacing = c(2, 2),
            padding = c(1, 2, 3)
        ),
        "must have length 1 or 2",
        fixed = TRUE
    )
})

test_that("snic_grid() handles rectangular grids with single seed per dimension", {
    img <- array(0, dim = c(10L, 10L, 1L))
    seeds <- snic_grid(
        img,
        type = "rectangular",
        spacing = c(50, 50),
        padding = c(1, 1)
    )

    expect_equal(nrow(seeds), 1L)
    expect_equal(as.numeric(seeds$r[1L]), mean(c(2, 9)))
    expect_equal(as.numeric(seeds$c[1L]), mean(c(2, 9)))
})

test_that("snic_grid(type = \"diamond\") respects asymmetric padding without recycling", {
    img <- array(0, dim = c(50L, 50L, 1L))
    seeds <- snic_grid(
        img,
        type = "diamond",
        spacing = c(8, 8),
        padding = c(1, 20)
    )

    expect_false(anyNA(seeds))
    expect_true(all(seeds$r >= 1 & seeds$r <= nrow(img)))
    expect_true(all(seeds$c >= 1 & seeds$c <= ncol(img)))
})

test_that("snic_grid(type = \"diamond\") filters use axis-specific padding", {
    img <- array(0, dim = c(40L, 60L, 1L))
    spacing <- c(7, 9)
    padding <- c(2, 18)

    expect_no_warning({
        seeds <- snic_grid(
            img,
            type = "diamond",
            spacing = spacing,
            padding = padding
        )
    })

    expect_gt(nrow(seeds), 0L)
    expect_true(all(seeds$r <= nrow(img) - padding[[1]]))
    expect_true(all(seeds$c <= ncol(img) - padding[[2]]))
})

test_that("snic_grid(type = \"hexagonal\") filters use axis-specific padding", {
    img <- array(0, dim = c(45L, 70L, 1L))
    spacing <- c(6, 5)
    padding <- c(3, 12)

    expect_no_warning({
        seeds <- snic_grid(
            img,
            type = "hexagonal",
            spacing = spacing,
            padding = padding
        )
    })

    expect_gt(nrow(seeds), 0L)
    expect_true(all(seeds$r <= nrow(img) - padding[[1]]))
    expect_true(all(seeds$c <= ncol(img) - padding[[2]]))
})

test_that("snic_grid(type = \"random\") validates inputs and interior area", {
    img <- array(0, dim = c(10L, 10L, 1L))

    expect_error(
        snic_grid(
            img,
            type = "random",
            spacing = numeric(0)
        ),
        "must have length 1 or 2",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "random",
            spacing = c(20, 20),
            padding = c(4.9, 4.9)
        ),
        "yields no valid seed positions",
        fixed = TRUE
    )
    expect_error(
        snic_grid(
            img,
            type = "random",
            spacing = c(0.2, 0.2),
            padding = c(0, 0)
        ),
        "argument 'spacing' must be greater than 1",
        fixed = TRUE
    )
})
