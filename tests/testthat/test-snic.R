grid_seed_count <- function(height, width, step) {
    length(seq(0, height - 1L, by = step)) *
        length(seq(0, width - 1L, by = step))
}

seg_matrix_view <- function(segs, height, width) {
    expect_true(is.array(segs))
    expect_equal(dim(segs), c(height, width, 1L))
    segs[, , 1L]
}

test_that("grid seeding respects spacing and mask", {
    height <- 6L
    width <- 6L
    step <- 2L
    img <- array(runif(height * width), dim = c(height, width, 1L))
    # introduce NA at a grid location
    na_row <- 3L
    na_col <- 5L
    img[na_row, na_col, 1L] <- NA_real_
    seeds <- snic_seeds_grid(img, grid_step = step)
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
    img <- array(runif(height * width), dim = c(height, width, 1L))
    rast <- terra::rast(nrows = height, ncols = width, nlyrs = 1)
    terra::values(rast) <- as.numeric(aperm(img, c(2, 1, 3)))

    seeds_grid_mat <- snic_seeds_grid(img, grid_step = step)
    seeds_grid_rast <- snic_seeds_grid(rast, grid_step = step)
    expect_equal(seeds_grid_rast, seeds_grid_mat)
})

test_that("snic works with user supplied seeds", {
    height <- 4L
    width <- 4L
    set.seed(2)
    img <- array(runif(height * width), dim = c(height, width, 1L))
    seeds <- snic_seeds_grid(img, grid_step = 2L)
    segs <- snic(img, seeds = seeds, compactness = 5)
    seg_mat <- seg_matrix_view(segs, height, width)
    expect_true(all(seg_mat[!is.na(seg_mat)] >= 1L))
    expect_true(all(seg_mat[!is.na(seg_mat)] <= nrow(seeds)))
})

test_that("snic default seeding matches grid helper", {
    height <- 6L
    width <- 6L
    step <- 3L
    set.seed(5)
    img <- array(runif(height * width), dim = c(height, width, 1L))
    grid_seeds <- snic_seeds_grid(img, grid_step = step)
    seg_manual <- snic(img, seeds = grid_seeds)
    seg_default <- snic(img, grid_step = step)
    expect_equal(seg_default, seg_manual)
})

test_that("snic skips NA pixels", {
    height <- 5L
    width <- 5L
    set.seed(4)
    img <- array(runif(height * width * 2L), dim = c(height, width, 2L))
    na_row <- 2L
    na_col <- 4L
    img[na_row, na_col, ] <- NA_real_
    seeds <- snic_seeds_grid(img, grid_step = 2L)
    segs <- snic(img, seeds = seeds, compactness = 10)
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
    rgb <- array(runif(height * width * 3L), dim = c(height, width, 3L))
    rgb_mat <- matrix(as.numeric(rgb), ncol = 3L, byrow = FALSE)
    lab_mat <- grDevices::convertColor(
        rgb_mat,
        from = "sRGB",
        to = "Lab"
    )
    lab <- array(lab_mat, dim = c(height, width, 3L))
    segs <- snic(lab, grid_step = 3L, compactness = 15)
    seg_mat <- seg_matrix_view(segs, height, width)
    expect_true(any(!is.na(seg_mat)))
    expect_true(all(seg_mat[!is.na(seg_mat)] >= 1L))
})

test_that("snic handles SpatRaster input", {
    skip_if_not_installed("terra")
    height <- 6L
    width <- 6L
    set.seed(15)
    rgb <- array(runif(height * width * 3L), dim = c(height, width, 3L))
    rast <- terra::rast(nrows = height, ncols = width, nlyrs = 3L)
    terra::values(rast) <- matrix(
        as.numeric(aperm(rgb, c(2, 1, 3))),
        ncol = 3L,
        byrow = FALSE
    )
    seg_vec <- snic(rgb, compactness = 12, grid_step = 3L)
    seg_from_rast <- snic(rast, compactness = 12, grid_step = 3L)
    expect_true(is.array(seg_from_rast))
    expect_equal(dim(seg_from_rast), c(height, width, 1L))
    expect_equal(seg_from_rast, seg_vec)
})


test_that("plot_snic_seeds renders without error", {
    height <- 4L
    width <- 4L
    img <- array(seq_len(height * width), dim = c(height, width, 1L))
    seeds <- snic_seeds_grid(img, grid_step = 2L)
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off())
    expect_silent(plot(img, seeds = seeds))
})

test_that("plot handles segmentation output matrices", {
    height <- 4L
    width <- 4L
    set.seed(21)
    img <- array(runif(height * width), dim = c(height, width, 1L))
    segs <- snic(img, compactness = 5, grid_step = 2L)
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_silent(plot(segs))
})
