test_that("snic_grid_manual collects locator clicks into seed data frame", {
    skip_if_not_installed("terra")

    img <- terra::rast(
        nrows = 4, ncols = 5, nlyrs = 3, xmin = 0,
        xmax = 10, ymin = 0, ymax = 8
    )
    terra::values(img) <-
        rep(seq_len(terra::ncell(img)), times = terra::nlyr(img))

    initial_seeds <- matrix(c(2L, 3L), ncol = 2, byrow = TRUE)
    colnames(initial_seeds) <- c("r", "c")

    click_valid_1 <- list(x = 1, y = 7)
    click_outside <- list(x = -1, y = 5)
    click_valid_2 <- list(x = 8, y = 1)
    locator_values <- list(click_valid_1, click_outside, click_valid_2, NULL)

    locator_mock <- function(n = 1, ...) {
        stopifnot(n == 1)
        if (length(locator_values) == 0L) {
            return(NULL)
        }
        value <- locator_values[[1L]]
        locator_values <<- locator_values[-1L]
        value
    }

    seg_dummy <- img[[1L]]
    captured <- list(
        snic = list(),
        plotRGB = list(),
        plot = list(),
        polygons = list()
    )

    valid_clicks <- list(click_valid_1, click_valid_2)
    prepared_initial <- snic:::.as_seeds(initial_seeds)
    additions <- do.call(
        rbind,
        lapply(valid_clicks, function(pt) {
            r <- terra::rowFromY(img, pt$y)
            c <- terra::colFromX(img, pt$x)
            data.frame(
                r = as.integer(r),
                c = as.integer(c),
                stringsAsFactors = FALSE
            )
        })
    )
    expected_final <- rbind(prepared_initial, additions)

    old_options <- options(snic.force_interactive = TRUE)
    on.exit(options(old_options), add = TRUE)

    result <- with_mocked_bindings(
        {
            local_result <- NULL
            expect_message(
                {
                    local_result <- snic_grid_manual(
                        img,
                        seeds = initial_seeds,
                        compactness = 0.25,
                        r = 1L, g = 2L, b = 3L,
                        seg_plot_args = list(
                            border = "red",
                            col = NA,
                            lwd = 0.4
                        )
                    )
                    local_result
                },
                "Left-click to add points; press ESC or right-click to stop."
            )
            local_result
        },
        locator = locator_mock,
        par = function(...) {
            invisible(NULL)
        },
        points = function(...) {
            invisible(NULL)
        },
        .package = "graphics"
    ) %>%
        with_mocked_bindings(
            plotRGB = function(x, ...) {
                captured$plotRGB[[length(captured$plotRGB) + 1L]] <<-
                    list(x = x, args = list(...))
                invisible(NULL)
            },
            plot = function(x, ...) {
                captured$plot[[length(captured$plot) + 1L]] <<-
                    list(x = x, args = list(...))
                invisible(NULL)
            },
            as.polygons = function(x, ...) {
                captured$polygons[[length(captured$polygons) + 1L]] <<-
                    list(x = x, args = list(...))
                list(id = length(captured$polygons))
            },
            .package = "terra"
        ) %>%
        with_mocked_bindings(
            snic = function(img, seeds, compactness, ...) {
                captured$snic[[length(captured$snic) + 1L]] <<- list(
                    img = img,
                    seeds = seeds,
                    compactness = compactness
                )
                seg_dummy
            },
            .package = "snic"
        )

    expect_equal(result, expected_final)
    expect_equal(length(captured$snic), length(valid_clicks))
    expect_equal(length(captured$plotRGB), length(valid_clicks) + 1L)
    expect_equal(length(captured$plot), length(valid_clicks))
    expect_equal(length(captured$polygons), length(valid_clicks))

    expect_equal(
        captured$snic[[1L]]$seeds,
        expected_final[1:2, , drop = FALSE]
    )
    expect_equal(captured$snic[[2L]]$seeds, expected_final)
    expect_identical(captured$snic[[1L]]$compactness, 0.25)
    expect_identical(captured$snic[[2L]]$compactness, 0.25)

    expect_true(
        all(vapply(captured$plotRGB, function(call) {
            inherits(call$x, "SpatRaster")
        }, logical(1)))
    )
    expect_true(
        all(vapply(captured$plot, function(call) {
            call$args$add
        }, logical(1)))
    )
    expect_true(
        all(vapply(captured$polygons, function(call) {
            identical(call$args$dissolve, TRUE)
        }, logical(1)))
    )
    expect_true(
        all(vapply(captured$polygons, function(call) {
            identical(call$args$na.rm, TRUE)
        }, logical(1)))
    )

    expect_true(
        all(vapply(captured$plotRGB, function(call) {
            call$args$r == 1L
        }, logical(1)))
    )
    expect_true(
        all(vapply(captured$plotRGB, function(call) {
            call$args$g == 2L
        }, logical(1)))
    )
    expect_true(
        all(vapply(captured$plotRGB, function(call) {
            call$args$b == 3L
        }, logical(1)))
    )
    expect_true(
        all(vapply(captured$plot, function(call) {
            call$args$border == "red"
        }, logical(1)))
    )
})
