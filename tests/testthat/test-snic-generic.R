skip_if_terra_broken()

make_img <- function(h = 6L, w = 7L, b = 2L) {
    arr <- array(seq_len(h * w * b), dim = c(h, w, b))
    rast <- terra::rast(arr)
    list(arr = arr, rast = rast)
}

test_that("array vs SpatRaster: rc -> xy equivalence including borders and OOB", {
    img <- make_img(8L, 9L, 3L)
    arr <- img$arr
    rast <- img$rast

    seeds_rc <- data.frame(
        r = c(1L, nrow(arr), 4L, -1L, nrow(arr) + 1L),
        c = c(1L, ncol(arr), 5L, 0L, ncol(arr) + 2L)
    )

    xy_arr <- .rc_to_xy(arr, seeds_rc)
    xy_rast <- .rc_to_xy(rast, seeds_rc)
    expect_equal(xy_arr, xy_rast)
})

test_that("array vs SpatRaster: xy -> rc equivalence with negative, float, and border values", {
    img <- make_img(8L, 9L, 3L)
    arr <- img$arr
    rast <- img$rast

    seeds_xy <- data.frame(
        x = c(-2, -0.1, 0, 0.5, 1.1, 4.3, ncol(arr) - 0.5, ncol(arr), ncol(arr) + 1),
        y = c(-2, -0.1, 0, 0.5, 2.7, nrow(arr) - 0.5, nrow(arr), nrow(arr) + 0.1, 1.2)
    )

    rc_arr <- .xy_to_rc(arr, seeds_xy)
    rc_rast <- .xy_to_rc(rast, seeds_xy)
    expect_equal(rc_arr, rc_rast)
})

test_that("arrays: round-trip rc -> xy -> rc identity for in-bounds seeds", {
    img <- make_img(7L, 6L, 1L)
    arr <- img$arr

    seeds_rc <- expand.grid(r = c(1L, 3L, nrow(arr)), c = c(1L, 2L, ncol(arr)))
    seeds_rc <- data.frame(r = as.integer(seeds_rc$r), c = as.integer(seeds_rc$c))

    xy <- .rc_to_xy(arr, seeds_rc)
    rc_back <- .xy_to_rc(arr, xy)
    expect_equal(rc_back, seeds_rc)
})

test_that("arrays: wgs84 conversions must error", {
    arr <- make_img(4L, 5L, 1L)$arr
    expect_error(.wgs84_to_xy(arr, data.frame(lat = 0, lon = 0)))
    expect_error(.xy_to_wgs84(arr, data.frame(x = 0.5, y = 0.5)))
})

test_that("SpatRaster: round-trip conversions with defined CRS (WGS84) are consistent", {
    img <- make_img(6L, 5L, 1L)
    arr <- img$arr
    rast <- img$rast
    terra::crs(rast) <- "EPSG:3857"

    seeds_rc <- expand.grid(r = c(1L, 3L, nrow(arr)), c = c(1L, 2L, ncol(arr)))
    seeds_rc <- data.frame(r = as.integer(seeds_rc$r), c = as.integer(seeds_rc$c))

    # rc -> wgs84 -> rc
    rc_round <- .wgs84_to_rc(rast, .rc_to_wgs84(rast, seeds_rc))
    expect_equal(rc_round, seeds_rc)

    # wgs84 -> xy -> wgs84
    seeds_wgs <- .rc_to_wgs84(rast, seeds_rc)
    wgs_round <- .xy_to_wgs84(rast, .wgs84_to_xy(rast, seeds_wgs))
    expect_equal(wgs_round, seeds_wgs)

    # rc -> xy -> rc
    rc_round2 <- .xy_to_rc(rast, .rc_to_xy(rast, seeds_rc))
    expect_equal(rc_round2, seeds_rc)

    # wgs84 -> xy -> rc -> wgs84
    wgs_round2 <- .rc_to_wgs84(rast, .xy_to_rc(rast, .wgs84_to_xy(rast, seeds_wgs)))
    expect_equal(wgs_round2, seeds_wgs)
})

test_that("SpatRaster: wgs84 conversions error when CRS is missing", {
    img <- make_img(4L, 4L, 1L)
    rast <- img$rast
    terra::crs(rast) <- ""

    expect_error(.wgs84_to_xy(rast, data.frame(lat = 0, lon = 0)))
    expect_error(.xy_to_wgs84(rast, data.frame(x = 0.5, y = 0.5)))
})

test_that("Invalid seed structures raise errors for both backends", {
    img <- make_img(5L, 5L, 1L)
    arr <- img$arr
    rast <- img$rast

    # .rc_to_xy requires columns r, c
    bad_rc <- data.frame(a = 1, b = 2)
    expect_error(.rc_to_xy(arr, bad_rc))
    expect_error(.rc_to_xy(rast, bad_rc))

    # .xy_to_rc requires columns x, y
    bad_xy <- data.frame(a = 0.5, b = 0.5)
    expect_error(.xy_to_rc(arr, bad_xy))
    expect_error(.xy_to_rc(rast, bad_xy))
})
