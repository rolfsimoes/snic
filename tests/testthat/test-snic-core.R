make_image <- function(h = 6L, w = 7L, b = 2L) {
    array(seq_len(h * w * b), dim = c(h, w, b))
}

make_backends <- function(arr, crs = NULL) {
    backends <- list(array = arr)
    if (requireNamespace("terra", quietly = TRUE)) {
        rast <- terra::rast(arr)
        if (!is.null(crs)) {
            terra::crs(rast) <- crs
        }
        backends$raster <- rast
    }
    backends
}

test_that("snic produces identical segments for arrays and rasters", {
    skip_if_not_installed("terra")

    arr <- make_image(6L, 5L, 3L)
    backends <- make_backends(arr)
    seeds <- snic::snic_grid(
        arr,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )

    segs <- lapply(backends, function(x) {
        snic::snic(x, seeds = seeds, compactness = 0.25)
    })

    expect_equal(
        snic:::.x_to_arr(snic::snic_get_seg(segs$array)),
        snic:::.x_to_arr(snic::snic_get_seg(segs$raster)),
        ignore_attr = TRUE
    )
    expect_true(inherits(snic::snic_get_seg(segs$raster), "SpatRaster"))
    expect_equal(
        dim(snic:::.x_to_arr(snic::snic_get_seg(segs$raster))),
        c(nrow(arr), ncol(arr), 1L)
    )
})

test_that("SpatRaster inputs accept WGS84 seeds", {
    skip_if_not_installed("terra")

    arr <- make_image(5L, 4L, 2L)
    backends <- make_backends(arr, crs = "EPSG:4326")
    seeds_rc <- snic::snic_grid(
        arr,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    seeds_wgs84 <- snic:::.rc_to_wgs84(backends$raster, seeds_rc)

    seg_latlon <- snic::snic(backends$raster, seeds = seeds_wgs84, compactness = 0.3)
    seg_rc <- snic::snic(backends$raster, seeds = seeds_rc, compactness = 0.3)

    expect_equal(
        snic:::.x_to_arr(snic::snic_get_seg(seg_latlon)),
        snic:::.x_to_arr(snic::snic_get_seg(seg_rc))
    )
})

test_that("NA pixels remain NA after segmentation for every backend", {
    skip_if_not_installed("terra")

    arr <- make_image(5L, 5L, 2L)
    na_positions <- matrix(
        c(
            2L, 3L,
            4L, 1L
        ),
        ncol = 2L,
        byrow = TRUE
    )
    for (i in seq_len(nrow(na_positions))) {
        arr[na_positions[i, 1L], na_positions[i, 2L], ] <- NA_real_
    }

    backends <- make_backends(arr)
    seeds <- data.frame(
        r = c(1L, 3L, 5L),
        c = c(1L, 3L, 5L)
    )

    mask_idx <- matrix(FALSE, nrow = nrow(arr), ncol = ncol(arr))
    for (i in seq_len(nrow(na_positions))) {
        mask_idx[na_positions[i, 1L], na_positions[i, 2L]] <- TRUE
    }

    for (kind in names(backends)) {
        seg <- snic::snic(backends[[kind]], seeds = seeds, compactness = 0.2)
        seg_mat <- snic:::.x_to_arr(snic::snic_get_seg(seg))
        mask <- is.na(seg_mat)
        expect_true(all(mask[mask_idx]))
        expect_false(any(mask[!mask_idx]))
    }
})

test_that("integer inputs are coerced without affecting results", {
    arr_int <- array(as.integer(seq_len(4L * 4L * 2L)), dim = c(4L, 4L, 2L))
    arr_double <- array(as.numeric(arr_int), dim = dim(arr_int))
    seeds <- snic::snic_grid(
        arr_double,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )

    seg_double <- snic::snic(arr_double, seeds = seeds, compactness = 0)
    seg_int <- snic::snic(arr_int, seeds = seeds, compactness = 0)
    expect_equal(
        snic::snic_get_seg(seg_int),
        snic::snic_get_seg(seg_double)
    )
})

test_that("snic validates malformed or missing seed inputs", {
    img <- make_image(4L, 4L, 1L)

    bad_df <- data.frame(value = 1:2)
    expect_error(
        snic::snic(img, seeds = bad_df),
        "must have columns"
    )

    bad_matrix <- matrix(1:9, ncol = 3)
    expect_error(
        snic::snic(img, seeds = bad_matrix),
        "must have exactly two columns"
    )

    empty_matrix <- matrix(numeric(0), ncol = 2)
    expect_error(
        snic::snic(img, seeds = empty_matrix),
        "must contain at least one coordinate"
    )

    expect_error(
        snic::snic(img, seeds = NULL),
        "must contain at least one coordinate"
    )
})
