skip_if_not_installed("terra")

make_grid_inputs <- function(h = 14L, w = 15L, b = 3L, with_crs = TRUE) {
    arr <- array(seq_len(h * w * b), dim = c(h, w, b))
    rast <- terra::rast(arr)
    if (with_crs) {
        terra::crs(rast) <- "EPSG:3857"
    } else {
        terra::crs(rast) <- ""
    }
    list(array = arr, SpatRaster = rast)
}

with_seed <- function(seed, code) {
    stopifnot(is.function(code))
    seed_env <- globalenv()
    has_seed <- exists(".Random.seed", envir = seed_env, inherits = FALSE)
    if (has_seed) {
        old_seed <- get(".Random.seed", envir = seed_env, inherits = FALSE)
        on.exit(assign(".Random.seed", old_seed, envir = seed_env), add = TRUE)
    } else {
        on.exit({
            if (exists(".Random.seed", envir = seed_env, inherits = FALSE)) {
                rm(list = ".Random.seed", envir = seed_env)
            }
        }, add = TRUE)
    }
    set.seed(seed)
    code()
}

expect_grid_error <- function(expr_fn, regexp, imgs, fixed = TRUE) {
    for (nm in names(imgs)) {
        expect_error(expr_fn(imgs[[nm]]), regexp, fixed = fixed, info = nm)
    }
}

expect_grid_consistency <- function(type,
                                    spacing,
                                    padding = spacing / 2,
                                    dims = c(14L, 15L, 3L),
                                    with_crs = TRUE,
                                    seed = NULL) {
    imgs <- make_grid_inputs(
        h = dims[[1]],
        w = dims[[2]],
        b = dims[[3]],
        with_crs = with_crs
    )

    grid_call <- function(x) {
        snic_grid(x,
            type = type,
            spacing = spacing,
            padding = padding
        )
    }
    arr_seeds <- if (is.null(seed)) {
        grid_call(imgs$array)
    } else {
        with_seed(seed, function() grid_call(imgs$array))
    }
    rast_seeds <- if (is.null(seed)) {
        grid_call(imgs$SpatRaster)
    } else {
        with_seed(seed, function() grid_call(imgs$SpatRaster))
    }

    arr_xy <- .rc_to_xy(imgs$SpatRaster, arr_seeds)
    arr_rc <- .xy_to_rc(imgs$SpatRaster, arr_xy)
    if (with_crs) {
        rast_rc <- .wgs84_to_rc(imgs$SpatRaster, rast_seeds)
        expect_equal(arr_rc, rast_rc)
    } else {
        rast_rc <- rast_seeds
        expect_equal(arr_seeds, rast_rc)
    }

    list(
        arr = arr_seeds,
        arr_rc = arr_rc,
        rast = rast_seeds,
        rast_rc = rast_rc,
        imgs = imgs
    )
}

test_that("snic_grid requires explicit spacing for both backends", {
    imgs <- make_grid_inputs()
    expect_error(
        snic_grid(imgs$array),
        "argument \"spacing\" is missing, with no default",
        fixed = TRUE,
        info = "array"
    )
    expect_error(
        snic_grid(imgs$SpatRaster),
        "argument \"spacing\" is missing, with no default",
        fixed = TRUE,
        info = "SpatRaster"
    )
})

test_that("grid helpers enforce minimum image dimensions", {
    arr <- array(0, dim = c(0L, 5L, 1L))
    rast <- terra::rast(arr)
    imgs <- list(array = arr, SpatRaster = rast)
    expect_grid_error(
        function(x) snic_grid(x, spacing = 2, padding = c(0, 0)),
        "argument 'x' must have at least one row and one column",
        imgs
    )
})

test_that("grid helpers validate spacing inputs for all backends", {
    imgs <- make_grid_inputs()
    expect_invalid <- function(spacing, msg) {
        expect_grid_error(
            function(x) snic_grid(x,
                type = "rectangular",
                spacing = spacing,
                padding = c(0, 0)
            ),
            msg,
            imgs
        )
        expect_grid_error(
            function(x) snic_count_seeds(x,
                spacing = spacing,
                padding = c(0, 0)
            ),
            msg,
            imgs
        )
    }

    expect_invalid(numeric(0), "argument 'spacing' must have length 1 or 2")
    expect_invalid("a", "argument 'spacing' must be numeric")
    expect_invalid(c(2, NA), "argument 'spacing' must contain only finite values")
    expect_invalid(c(1, 3), "argument 'spacing' must be greater than 1")
    expect_invalid(c(2, 2, 3), "argument 'spacing' must have length 1 or 2")
})

test_that("grid helpers validate padding inputs for all backends", {
    imgs <- make_grid_inputs()
    expect_invalid <- function(padding, msg) {
        expect_grid_error(
            function(x) snic_grid(x,
                type = "rectangular",
                spacing = c(3, 3),
                padding = padding
            ),
            msg,
            imgs
        )
        expect_grid_error(
            function(x) snic_count_seeds(x,
                spacing = c(3, 3),
                padding = padding
            ),
            msg,
            imgs
        )
    }

    expect_invalid("a", "argument 'padding' must be numeric")
    expect_invalid(c(0, NA), "argument 'padding' must contain only finite values")
    expect_invalid(c(-1, 0), "argument 'padding' must be non-negative")
    expect_invalid(c(10, 10), "argument 'padding' leaves no room for seed placement")
    expect_invalid(c(1, 2, 3), "argument 'padding' must have length 1 or 2")
})

test_that("grid helpers detect spacing/padding combinations that leave no interior points", {
    imgs <- make_grid_inputs(h = 6L, w = 8L, b = 1L)
    spacing <- c(2, 2)
    padding <- c(2.9, 1.6)
    msg <- "Spacing/padding combination yields no valid seed positions"

    expect_grid_error(
        function(x) snic_grid(x,
            type = "rectangular",
            spacing = spacing,
            padding = padding
        ),
        msg,
        imgs
    )
    expect_grid_error(
        function(x) snic_grid(x,
            type = "random",
            spacing = spacing,
            padding = padding
        ),
        msg,
        imgs
    )
    counts <- vapply(
        imgs,
        function(x) snic_count_seeds(x,
            spacing = spacing,
            padding = padding
        ),
        numeric(1)
    )
    expect_true(all(counts == 0))
})

test_that("rectangular grids match counts and spacing for arrays and rasters", {
    spacing <- c(3, 4)
    padding <- c(1, 2)
    res <- expect_grid_consistency(
        "rectangular",
        spacing = spacing,
        padding = padding,
        dims = c(18L, 20L, 2L)
    )

    expect_identical(colnames(res$arr), c("r", "c"))
    expect_identical(colnames(res$rast), c("lat", "lon"))
    expect_equal(res$arr_rc, res$rast_rc)
    expect_true(all(is.finite(res$rast$lat)))
    expect_true(all(is.finite(res$rast$lon)))

    rows <- sort(unique(res$arr$r))
    cols <- sort(unique(res$arr$c))
    expect_true(length(rows) > 1)
    expect_true(length(cols) > 1)
    expect_true(all(diff(rows) == diff(rows)[1]))
    expect_true(all(diff(cols) == diff(cols)[1]))

    expect_equal(
        nrow(res$arr),
        snic_count_seeds(res$imgs$array, spacing = spacing, padding = padding)
    )
    expect_equal(
        nrow(res$arr),
        snic_count_seeds(res$imgs$SpatRaster, spacing = spacing, padding = padding)
    )
})

test_that("diamond grids add shifted points while staying within padded bounds", {
    spacing <- c(6, 5)
    padding <- c(2, 3)
    res <- expect_grid_consistency(
        "diamond",
        spacing = spacing,
        padding = padding,
        dims = c(32L, 28L, 2L)
    )

    h <- nrow(res$imgs$array)
    w <- ncol(res$imgs$array)
    expect_true(all(res$arr$r >= padding[[1]] + 1))
    expect_true(all(res$arr$r <= h - padding[[1]]))
    expect_true(all(res$arr$c >= padding[[2]] + 1))
    expect_true(all(res$arr$c <= w - padding[[2]]))

    base_rect <- snic_grid(
        res$imgs$array,
        type = "rectangular",
        spacing = spacing * sqrt(2),
        padding = padding
    )
    expect_gt(nrow(res$arr), nrow(base_rect))
})

test_that("hexagonal grids generate additional offset rows for both backends", {
    spacing <- c(5, 4)
    padding <- c(1, 2)
    res <- expect_grid_consistency(
        "hexagonal",
        spacing = spacing,
        padding = padding,
        dims = c(24L, 30L, 2L)
    )

    h <- nrow(res$imgs$array)
    w <- ncol(res$imgs$array)
    expect_true(all(res$arr$r >= padding[[1]] + 1))
    expect_true(all(res$arr$r <= h - padding[[1]]))
    expect_true(all(res$arr$c >= padding[[2]] + 1))
    expect_true(all(res$arr$c <= w - padding[[2]]))

    base_rect <- snic_grid(
        res$imgs$array,
        type = "rectangular",
        spacing = spacing * c(1, sqrt(3)),
        padding = padding
    )
    expect_gt(nrow(res$arr), nrow(base_rect))
})

test_that("random grids are reproducible, unique, and match count estimates", {
    spacing <- c(4, 5)
    padding <- c(1.5, 2.5)
    dims <- c(18L, 16L, 3L)
    res <- expect_grid_consistency(
        "random",
        spacing = spacing,
        padding = padding,
        dims = dims,
        seed = 99
    )

    expect_equal(res$arr_rc, res$rast_rc)
    expect_equal(nrow(res$arr), nrow(unique(res$arr)))
    expect_equal(
        nrow(res$arr),
        snic_count_seeds(res$imgs$array, spacing = spacing, padding = padding)
    )
    expect_equal(
        nrow(res$arr),
        snic_count_seeds(res$imgs$SpatRaster, spacing = spacing, padding = padding)
    )

    h <- nrow(res$imgs$array)
    w <- ncol(res$imgs$array)
    expect_true(all(res$arr$r >= padding[[1]] + 1))
    expect_true(all(res$arr$r <= h - padding[[1]]))
    expect_true(all(res$arr$c >= padding[[2]] + 1))
    expect_true(all(res$arr$c <= w - padding[[2]]))
})

test_that("SpatRaster inputs without CRS return pixel coordinates", {
    res <- expect_grid_consistency(
        "rectangular",
        spacing = c(3, 3),
        padding = c(1, 1),
        dims = c(10L, 9L, 2L),
        with_crs = FALSE
    )

    expect_identical(colnames(res$rast), c("r", "c"))
    expect_equal(res$arr, res$rast)
})

test_that("single seed per dimension is centered for both backends", {
    spacing <- c(50, 60)
    padding <- c(1, 2)
    dims <- c(12L, 15L, 1L)
    res <- expect_grid_consistency(
        "rectangular",
        spacing = spacing,
        padding = padding,
        dims = dims
    )

    expect_equal(nrow(res$arr), 1L)

    expected_r <- mean(c(padding[[1]] + 1, dims[[1]] - padding[[1]]))
    expected_c <- mean(c(padding[[2]] + 1, dims[[2]] - padding[[2]]))
    expect_equal(res$arr$r[[1]], expected_r)
    expect_equal(res$arr$c[[1]], expected_c)
})
