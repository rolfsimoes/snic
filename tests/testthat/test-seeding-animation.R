test_that("snic_animation creates gif from sequential seeds", {
    skip_if_not_installed("terra")
    skip_if_not_installed("magick")
    terra::terraOptions(progress = 0)

    img <- terra::rast(nrows = 8, ncols = 8, nlyrs = 3, xmin = 0, xmax = 8, ymin = 0, ymax = 8)
    terra::values(img) <- runif(terra::ncell(img) * terra::nlyr(img))

    seeds <- matrix(
        c(
            2L, 2L,
            4L, 4L,
            6L, 6L,
            7L, 2L
        ),
        ncol = 2,
        byrow = TRUE
    )
    colnames(seeds) <- c("r", "c")

    tmp <- file.path(tempdir(), paste0("seeding-animation-", Sys.getpid()))
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    old <- setwd(tmp)
    on.exit(
        {
            setwd(old)
            unlink(tmp, recursive = TRUE)
        },
        add = TRUE
    )

    expect_message(
        gif_path <- snic_animation(
            img,
            seeds = seeds,
            compactness = 0.2,
            r = 1L,
            g = 2L,
            b = 3L
        ),
        "Saved animation to"
    )

    expect_true(file.exists(gif_path))

    gif <- magick::image_read(gif_path)
    expect_equal(length(gif), min(nrow(seeds), 10L))

    unlink(gif_path)
})

test_that("snic_animation validates inputs", {
    skip_if_not_installed("terra")

    img <- terra::rast(
        nrows = 4,
        ncols = 4,
        nlyrs = 1,
        xmin = 0,
        xmax = 4,
        ymin = 0,
        ymax = 4
    )

    expect_error(
        snic_animation(img, seeds = NULL),
        "argument 'seeds' cannot be NULL"
    )

    expect_error(
        snic_animation(matrix(1), seeds = matrix(c(1, 1), ncol = 2)),
        "no applicable method for 'snic_animation' applied to an object of class"
    )
})
