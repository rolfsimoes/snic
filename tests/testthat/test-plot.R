skip_if_terra_broken()

make_plot_fixture <- function(h = 6L, w = 7L, b = 3L, crs = "EPSG:4326") {
    arr <- array(seq_len(h * w * b), dim = c(h, w, b))
    rast <- terra::rast(arr)
    if (!is.null(crs)) {
        terra::crs(rast) <- crs
    }
    list(array = arr, raster = rast)
}

to_spatraster <- function(x) {
    if (inherits(x, "SpatRaster")) {
        return(x)
    }
    snic:::.arr_to_x(snic:::.rast_tmpl(x), snic:::.x_to_arr(x))
}

backend_bbox <- function(x) {
    if (inherits(x, "SpatRaster")) {
        return(c(
            terra::xmin(x),
            terra::xmax(x),
            terra::ymin(x),
            terra::ymax(x)
        ))
    }
    c(0, dim(x)[[2L]], 0, dim(x)[[1L]])
}

capture_usr <- function(expr) {
    tmp <- tempfile(fileext = ".png")
    grDevices::png(filename = tmp, width = 200, height = 200)
    on.exit(grDevices::dev.off(), add = TRUE)
    on.exit(unlink(tmp), add = TRUE)
    eval(substitute(expr), envir = parent.frame())
    graphics::par("usr")
}

capture_seed_points <- function(expr) {
    tmp <- tempfile(fileext = ".png")
    grDevices::png(filename = tmp, width = 200, height = 200)
    on.exit(grDevices::dev.off(), add = TRUE)
    on.exit(unlink(tmp), add = TRUE)
    captured <- NULL
    testthat::with_mocked_bindings(
        {
            eval(substitute(expr), envir = parent.frame())
        },
        points = function(x, y, ...) {
            captured <<- list(x = x, y = y, args = list(...))
            invisible(NULL)
        },
        .package = "graphics"
    )
    captured
}

test_that("snic_plot rejects unsupported object types", {
    expect_error(
        snic_plot(list()),
        "no applicable method",
        fixed = FALSE
    )
})

test_that("invalid bands, seeds, and segments fail for both backends", {
    fixture <- make_plot_fixture()
    backends <- list(array = fixture$array, raster = fixture$raster)

    for (kind in names(backends)) {
        x <- backends[[kind]]
        n_bands <- if (inherits(x, "SpatRaster")) terra::nlyr(x) else dim(x)[[3L]]

        expect_error(
            snic_plot(x, band = n_bands + 1L),
            "invalid band index",
            ignore.case = TRUE
        )

        bad_seeds <- data.frame(value = 1:2)
        expect_error(
            snic_plot(x, seeds = bad_seeds),
            "must have columns",
            ignore.case = TRUE
        )

        expect_error(snic_plot(x, seg = list()))
    }
})

test_that("snic_plot accepts band names for SpatRaster inputs", {
    fixture <- make_plot_fixture()
    names(fixture$raster) <- c("B02", "B04", "B08")

    expect_silent(snic_plot(fixture$raster, band = "B02"))
    expect_silent(snic_plot(fixture$raster, r = "B08", g = "B04", b = "B02"))
})

test_that("RGB plots honor the raster extent", {
    fixture <- make_plot_fixture()
    backends <- list(array = fixture$array, raster = fixture$raster)

    for (kind in names(backends)) {
        x <- backends[[kind]]
        template <- to_spatraster(x)
        expected <- capture_usr({
            terra::plotRGB(
                template,
                r = 1L,
                g = 2L,
                b = 3L,
                mar = 0,
                smooth = FALSE,
                stretch = "lin",
                axes = FALSE,
                maxcell = 100000L
            )
        })
        usr <- capture_usr({
            snic_plot(x, r = 1L, g = 2L, b = 3L)
        })
        expect_equal(usr, expected, tolerance = 1e-6)
    }
})

test_that("seed overlays align for rc, xy, and wgs84 inputs", {
    fixture <- make_plot_fixture(crs = "EPSG:3857")
    backends <- list(array = fixture$array, raster = fixture$raster)

    for (kind in names(backends)) {
        x <- backends[[kind]]
        template <- to_spatraster(x)
        seeds_rc <- .seeds(r = c(1L, nrow(template)), c = c(1L, ncol(template)))
        xy_expected <- snic:::as_seeds_xy(seeds_rc, template)
        xy_seeds <- xy_expected
        wgs_seeds <- snic:::.rc_to_wgs84(template, seeds_rc)

        points_rc <- capture_seed_points({
            snic_plot(x, band = 1L, seeds = seeds_rc)
        })
        points_xy <- capture_seed_points({
            snic_plot(x, band = 1L, seeds = xy_seeds)
        })
        points_wgs <- capture_seed_points({
            snic_plot(x, band = 1L, seeds = wgs_seeds)
        })

        expect_equal(points_rc$x, xy_expected$x, tolerance = 1e-6)
        expect_equal(points_rc$y, xy_expected$y, tolerance = 1e-6)
        expect_equal(points_xy$x, xy_expected$x, tolerance = 1e-6)
        expect_equal(points_xy$y, xy_expected$y, tolerance = 1e-6)
        expect_equal(points_wgs$x, xy_expected$x, tolerance = 1e-6)
        expect_equal(points_wgs$y, xy_expected$y, tolerance = 1e-6)
    }
})
