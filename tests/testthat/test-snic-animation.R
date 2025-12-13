test_that("snic_animation creates gif from sequential seeds", {
    skip_if_terra_broken()
    skip_if_not_installed("magick")

    terra::terraOptions(progress = 0)
    img <- terra::rast(
        nrows = 8, ncols = 8, nlyrs = 3,
        xmin = 0, xmax = 8, ymin = 0, ymax = 8
    )
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

    tmp_dir <- tempfile("snic-animation-")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    output_file <- normalizePath(
        file.path(tmp_dir, "animation.gif"),
        mustWork = FALSE
    )

    expect_message(
        gif_path <- snic_animation(
            img,
            seeds = seeds,
            file_path = output_file,
            snic_args = list(compactness = 0.2),
            r = 1L,
            g = 2L,
            b = 3L
        ),
        "Saved animation to"
    )

    expect_true(file.exists(gif_path))
    expect_identical(gif_path, output_file)

    gif <- magick::image_read(gif_path)
    expect_equal(length(gif), min(nrow(seeds), 10L))

    unlink(gif_path)
})

test_that("snic_animation validates inputs", {
    skip_if_terra_broken()
    skip_if_not_installed("magick")

    img <- terra::rast(
        nrows = 4,
        ncols = 4,
        nlyrs = 1,
        xmin = 0,
        xmax = 4,
        ymin = 0,
        ymax = 4
    )
    terra::values(img) <- runif(terra::ncell(img) * terra::nlyr(img))

    expect_error(
        snic_animation(img, seeds = NULL, file_path = tempfile(fileext = ".gif")),
        "argument 'seeds' cannot be NULL"
    )

    seeds_invalid <- matrix(c(1, 1), ncol = 2)
    colnames(seeds_invalid) <- c("r", "c")
    expect_error(
        snic_animation(matrix(1), seeds = seeds_invalid, file_path = tempfile(fileext = ".gif")),
        "argument 'x' must have 3 dimensions",
        fixed = TRUE
    )

    seeds <- matrix(c(1L, 1L), ncol = 2)
    colnames(seeds) <- c("r", "c")
    expect_error(
        snic_animation(img, seeds = seeds, delay = 0, file_path = tempfile(fileext = ".gif")),
        "argument 'delay' must be a positive number"
    )
})
