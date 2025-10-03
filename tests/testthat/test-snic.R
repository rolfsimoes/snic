test_that("snic basic small image works", {
    set.seed(123)
    # create a 2x2 image with 1 band
    height <- 2L
    width <- 2L
    k <- 2L
    n_pix <- height * width
    # use byrow=TRUE to fill matrix row-wise for clarity
    img <- matrix(c(10, 12, 20, 25), nrow = n_pix, ncol = 1, byrow = TRUE)
    segs <- snic(img, width = width, height = height, k = k)
    expect_equal(length(segs), n_pix)
    expect_true(all(segs %in% seq_len(k)))
    # should produce exactly k clusters
    expect_equal(length(unique(segs)), k)
})

test_that("snic works with multi-band image", {
    # simulate 6-band image of size 4x5
    height <- 4L
    width <- 5L
    k <- 5L
    n_pix <- height * width
    n_bands <- 6L
    set.seed(42)
    img <- matrix(runif(n_pix * n_bands), nrow = n_pix, ncol = n_bands)
    segs <- snic(img, width = width, height = height, k = k)
    # correct length
    expect_equal(length(segs), n_pix)
    # labels within range 1..k
    expect_true(all(segs >= 1L & segs <= k))
    # number of unique labels <= k
    expect_true(length(unique(segs)) <= k)
})

test_that("snic skips pixels that contain NA values", {
    height <- 3L
    width <- 3L
    k <- 3L
    n_pix <- height * width
    n_bands <- 2L
    set.seed(99)
    img <- matrix(runif(n_pix * n_bands), nrow = n_pix, ncol = n_bands)
    img[5, 1] <- NA_real_
    segs <- snic(img, width = width, height = height, k = k)
    expect_equal(length(segs), n_pix)
    expect_true(is.na(segs[5]))
    expect_true(all(!is.na(segs[-5])))
    expect_true(all(segs[!is.na(segs)] >= 1L & segs[!is.na(segs)] <= k))
})

test_that("snic supports 8-neighbour connectivity", {
    height <- 3L
    width <- 3L
    k <- 2L
    n_pix <- height * width
    img <- matrix(c(
        1, 2, 3,
        2, 2, 2,
        3, 2, 1
    ), nrow = n_pix, ncol = 1, byrow = TRUE)

    segs4 <- snic(img, width = width, height = height, k = k, connectivity = 4L)
    segs8 <- snic(img, width = width, height = height, k = k, connectivity = 8L)

    expect_equal(length(segs8), n_pix)
    expect_true(all(segs8[!is.na(segs8)] >= 1L & segs8[!is.na(segs8)] <= k))
    # 8-neighbourhood should not produce more clusters than 4-neighbourhood
    expect_lte(length(unique(segs8[!is.na(segs8)])), length(unique(segs4[!is.na(segs4)])))
})

test_that("invalid connectivity is rejected", {
    img <- matrix(runif(4L), nrow = 4L)
    expect_error(
        snic(img, width = 2L, height = 2L, k = 2L, connectivity = 6L),
        "connectivity must be 4 or 8"
    )
})
