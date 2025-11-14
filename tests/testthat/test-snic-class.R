test_that("snic arrays preserve class and expose accessors", {
    img <- array(seq_len(4L * 4L * 2L), dim = c(4L, 4L, 2L))
    seeds <- snic_grid(
        img,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )

    seg <- snic(img, seeds = seeds, compactness = 0)

    expect_s3_class(seg, c("snic", "array"))

    vals <- snic_values(seg)
    centers <- snic_centers(seg)

    expect_identical(vals, attr(seg, "values", exact = TRUE))
    expect_identical(centers, attr(seg, "centers", exact = TRUE))
    expect_equal(nrow(vals), nrow(seeds))
    expect_equal(ncol(vals), dim(img)[3L])
    expect_equal(nrow(centers), nrow(seeds))
    expect_equal(ncol(centers), 2L)

    not_snic <- array(1, dim = c(2L, 2L, 1L))
    expect_error(snic_values(not_snic), "expects an object")
    expect_error(snic_centers(not_snic), "expects an object")
})

test_that("print.snic mirrors base array printing", {
    img <- array(seq_len(3L * 3L), dim = c(3L, 3L, 1L))
    seeds <- snic_grid(
        img,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    seg <- snic(img, seeds = seeds, compactness = 0)

    base_seg <- seg
    attr(base_seg, "values") <- NULL
    attr(base_seg, "centers") <- NULL
    class(base_seg) <- setdiff(class(base_seg), "snic")

    expect_identical(capture.output(print(seg)), capture.output(print(base_seg)))
    expect_s3_class(seg, c("snic", "array"))
})
