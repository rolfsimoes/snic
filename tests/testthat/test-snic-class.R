test_that("snic arrays preserve class and expose accessors", {
    img <- array(seq_len(4L * 4L * 2L), dim = c(4L, 4L, 2L))
    seeds <- snic::snic_grid(
        img,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )

    seg <- snic::snic(img, seeds = seeds, compactness = 0)

    expect_s3_class(seg, "snic")

    vals <- seg$values
    centers <- seg$centers

    expect_equal(nrow(vals), nrow(seeds))
    expect_equal(ncol(vals), dim(img)[3L])
    expect_equal(nrow(centers), nrow(seeds))
    expect_equal(ncol(centers), 2L)

    not_snic <- array(1, dim = c(2L, 2L, 1L))
    expect_error(snic::snic_get_means(not_snic), "no applicable method")
    expect_error(snic::snic_get_centroids(not_snic), "no applicable method")
})

test_that("print.snic summarizes segmentation and returns input invisibly", {
    img <- array(seq_len(3L * 3L), dim = c(3L, 3L, 1L))
    seeds <- snic::snic_grid(
        img,
        type = "rectangular",
        spacing = c(2L, 2L),
        padding = c(0L, 0L)
    )
    seg <- snic::snic(img, seeds = seeds, compactness = 0)

    expect_snapshot_output(print(seg))

    capture.output(printed <- print(seg))
    expect_s3_class(printed, "snic")
    expect_identical(printed, seg)
})
