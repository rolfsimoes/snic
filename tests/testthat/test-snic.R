grid_seed_count <- function(height, width, step) {
    length(seq(0, height - 1, by = step)) *
        length(seq(0, width - 1, by = step))
}

seg_matrix_view <- function(segs, height, width) {
    expect_true(is.matrix(segs))
    expect_equal(dim(segs), c(height, width))
    segs
}

test_that("grid seeding respects spacing and mask", {
    height <- 6L
    width <- 6L
    step <- 2L
    img <- matrix(runif(height * width), nrow = height * width, ncol = 1)
    # introduce NA at a grid location
    na_row <- 3L
    na_col <- 5L
    img[(na_row - 1L) * width + na_col, 1] <- NA_real_
    seeds <- snic_seeds_grid(img, width, height, step)
    start_offset <- step %/% 2L + 1L
    expect_true(all((seeds[, 1L] - start_offset) %% step == 0L))
    expect_true(all((seeds[, 2L] - start_offset) %% step == 0L))
    expect_false(any(seeds[, 1L] == na_row & seeds[, 2L] == na_col))
    max_possible <- grid_seed_count(height, width, step)
    expect_lte(nrow(seeds), max_possible)
})

test_that("snic seeding helpers accept SpatRaster input", {
    skip_if_not_installed("terra")
    height <- 6L
    width <- 6L
    step <- 2L
    img <- matrix(runif(height * width), nrow = height * width, ncol = 1)
    rast <- terra::rast(nrows = height, ncols = width, nlyrs = 1)
    terra::values(rast) <- img

    seeds_grid_mat <- snic_seeds_grid(img, width, height, step)
    seeds_grid_rast <- snic_seeds_grid(rast, step = step)
    expect_equal(seeds_grid_rast, seeds_grid_mat)
})

test_that("snic works with user supplied seeds", {
    height <- 4L
    width <- 4L
    set.seed(2)
    img <- matrix(runif(height * width), nrow = height * width, ncol = 2)
    seeds <- snic_seeds_grid(img, width, height, step = 2L)
    segs <- snic(img, width, height, seeds = seeds, compactness = 5)
    expect_true(is.matrix(segs))
    expect_equal(dim(segs), c(height, width))
    expect_true(all(segs[!is.na(segs)] >= 1L))
    expect_true(all(segs[!is.na(segs)] <= nrow(seeds)))
})

test_that("snic default seeding matches grid helper", {
    height <- 6L
    width <- 6L
    step <- 3L
    set.seed(5)
    img <- matrix(runif(height * width), nrow = height * width, ncol = 1)
    grid_seeds <- snic_seeds_grid(img, width, height, step)
    seg_manual <- snic(img, width, height, seeds = grid_seeds)
    seg_default <- snic(img, width, height, grid_step = step)
    expect_equal(seg_default, seg_manual)
})

test_that("snic skips NA pixels", {
    height <- 5L
    width <- 5L
    set.seed(4)
    img <- matrix(runif(height * width * 2L), nrow = height * width, ncol = 2L)
    na_row <- 2L
    na_col <- 4L
    img[(na_row - 1L) * width + na_col, 1] <- NA_real_
    seeds <- snic_seeds_grid(img, width, height, step = 2L)
    segs <- snic(img, width, height,
        seeds = seeds,
        compactness = 10
    )
    seg_mat <- seg_matrix_view(segs, height, width)
    expect_true(is.na(seg_mat[na_row, na_col]))
    na_mask <- is.na(seg_mat)
    na_mask[na_row, na_col] <- FALSE
    expect_false(any(na_mask))
})

test_that("snic processes Lab-converted RGB input", {
    height <- 6L
    width <- 6L
    set.seed(10)
    rgb <- matrix(runif(height * width * 3L), ncol = 3L)
    lab <- snic::rgb2lab(rgb, min_value = rep(0, 3), max_value = rep(1, 3))
    segs <- snic(lab, width, height, grid_step = 3L, compactness = 15)
    expect_equal(dim(segs), c(height, width))
    expect_true(any(!is.na(segs)))
    expect_true(all(segs[!is.na(segs)] >= 1L))
})

test_that("snic handles SpatRaster input", {
    skip_if_not_installed("terra")
    height <- 6L
    width <- 6L
    set.seed(15)
    rgb <- matrix(runif(height * width * 3L), nrow = height * width, ncol = 3L)
    rast <- terra::rast(nrows = height, ncols = width, nlyrs = 3)
    terra::values(rast) <- rgb
    seg_vec <- snic(rgb, width, height, grid_step = 3L, compactness = 12)
    seg_from_rast <- snic(rast, grid_step = 3L, compactness = 12)
    expect_true(is.matrix(seg_from_rast))
    expect_equal(dim(seg_from_rast), c(height, width))
    expect_equal(seg_from_rast, seg_vec)
})


test_that("plot_snic_seeds renders without error", {
    height <- 4L
    width <- 4L
    img <- matrix(seq_len(height * width), nrow = height * width, ncol = 1)
    seeds <- snic_seeds_grid(img, width, height, step = 2L)
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off())
    expect_silent(plot(img, width = width, height = height, seeds = seeds))
})

test_that("plot handles segmentation output matrices", {
    height <- 4L
    width <- 4L
    set.seed(21)
    img <- matrix(runif(height * width), nrow = height * width, ncol = 1)
    segs <- snic(img, width, height, grid_step = 2L, compactness = 5)
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_silent(plot(segs, width = width, height = height))
})
