test_that("snic_rect_grid seeds align to requested spacing", {
    height <- 12L
    width <- 12L
    spacing <- c(3L, 3L)
    padding <- c(1L, 1L)
    img <- array(runif(height * width), dim = c(height, width, 1L))

    seeds <- snic_rect_grid(
        img,
        spacing = spacing,
        padding = padding
    )
    expect_equal(ncol(seeds), 2L)
    expect_equal(nrow(seeds), snic_count_seeds(img, spacing, padding))

    rows <- sort(unique(seeds[, 1L]))
    cols <- sort(unique(seeds[, 2L]))
    expect_true(all(diff(rows) == spacing[[1L]]))
    expect_true(all(diff(cols) == spacing[[2L]]))
})

test_that("snic seeding helpers accept SpatRaster input", {
    skip_if_not_installed("terra")
    height <- 6L
    width <- 7L
    img <- array(runif(height * width), dim = c(height, width, 1L))
    rast <- terra::rast(nrows = height, ncols = width, nlyrs = 1)
    terra::values(rast) <- as.numeric(aperm(img, c(2, 1, 3)))

    spacing <- c(2L, 2L)
    padding <- c(0L, 0L)

    seeds_array <- snic_rect_grid(
        img,
        spacing = spacing,
        padding = padding
    )

    seeds_rast <- snic_rect_grid(
        rast,
        spacing = spacing,
        padding = padding
    )
    expect_equal(seeds_rast, seeds_array)
})

test_that("snic works with user supplied seeds", {
    height <- 4L
    width <- 5L
    set.seed(2)
    img <- array(runif(height * width), dim = c(height, width, 1L))
    seeds <- snic_rect_grid(
        img,
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    segs <- snic(img, seeds = seeds, compactness = 5)
    expect_true(is.array(segs))
    expect_equal(dim(segs), c(height, width, 1L))
    seg_mat <- segs[, , 1L]
    expect_true(all(seg_mat[!is.na(seg_mat)] >= 1L))
    expect_true(all(seg_mat[!is.na(seg_mat)] <= nrow(seeds)))
})

test_that("snic errors when seeds are not supplied", {
    height <- 6L
    width <- 7L
    set.seed(5)
    img <- array(runif(height * width), dim = c(height, width, 1L))
    expect_error(
        snic(img),
        "argument \"seeds\" is missing, with no default"
    )
})

test_that("snic skips NA pixels", {
    height <- 5L
    width <- 7L
    set.seed(4)
    img <- array(runif(height * width * 2L), dim = c(height, width, 2L))
    na_row <- 2L
    na_col <- 4L
    img[na_row, na_col, ] <- NA_real_
    seeds <- snic_rect_grid(
        img,
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    segs <- snic(img, seeds = seeds, compactness = 10)
    expect_true(is.array(segs))
    expect_equal(dim(segs), c(height, width, 1L))
    seg_mat <- segs[, , 1L]
    expect_true(is.na(seg_mat[na_row, na_col]))
    na_mask <- is.na(seg_mat)
    na_mask[na_row, na_col] <- FALSE
    expect_false(any(na_mask))
})

test_that("snic processes Lab-converted RGB input", {
    height <- 6L
    width <- 7L
    set.seed(10)
    rgb <- array(runif(height * width * 3L), dim = c(height, width, 3L))
    rgb_mat <- matrix(as.numeric(rgb), ncol = 3L, byrow = FALSE)
    lab_mat <- grDevices::convertColor(
        rgb_mat,
        from = "sRGB",
        to = "Lab"
    )
    lab <- array(lab_mat, dim = c(height, width, 3L))
    seeds <- snic_hexagonal_grid(lab, spacing = 2L, padding = 0L)
    segs <- snic(lab, seeds = seeds, compactness = 15)
    expect_true(is.array(segs))
    expect_equal(dim(segs), c(height, width, 1L))
    seg_mat <- segs[, , 1L]
    expect_true(any(!is.na(seg_mat)))
    expect_true(all(seg_mat[!is.na(seg_mat)] >= 1L))
})

test_that("snic handles SpatRaster input", {
    skip_if_not_installed("terra")
    height <- 6L
    width <- 7L
    set.seed(15)
    rgb <- array(runif(height * width * 3L), dim = c(height, width, 3L))
    rast <- terra::rast(nrows = height, ncols = width, nlyrs = 3L)
    terra::values(rast) <- rgb
    seeds <- snic_rect_grid(
        rgb,
        spacing = c(3L, 3L),
        padding = c(0L, 0L)
    )
    seg_vec <- snic(rgb, seeds = seeds, compactness = 12)
    seg_from_rast <- snic(rast, seeds = seeds, compactness = 12)
    expect_true(inherits(seg_from_rast, "SpatRaster"))
    expect_equal(dim(seg_from_rast), c(height, width, 1L))
    expect_equal(c(terra::values(seg_from_rast)), c(aperm(seg_vec, c(2, 1, 3))))
})


test_that("snic_plot renders seeds without error", {
    height <- 4L
    width <- 4L
    img <- array(seq_len(height * width), dim = c(height, width, 1L))
    seeds <- snic_rect_grid(
        img,
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off())
    expect_silent(snic_plot(img, seeds = seeds))
})

test_that("snic_plot handles segmentation output arrays", {
    height <- 4L
    width <- 4L
    set.seed(21)
    img <- array(runif(height * width), dim = c(height, width, 1L))
    seeds <- snic_rect_grid(
        img,
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    segs <- snic(img, seeds = seeds, compactness = 5)
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_silent(snic_plot(segs))
})
