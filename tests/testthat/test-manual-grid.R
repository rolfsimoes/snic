test_that("snic_grid_manual collects locator clicks into seed data frame", {
    skip_if_terra_broken()

    img <- terra::rast(
        nrows = 4, ncols = 5, nlyrs = 3, xmin = 0,
        xmax = 10, ymin = 0, ymax = 8
    )
    terra::values(img) <-
        rep(seq_len(terra::ncell(img)), times = terra::nlyr(img))
    proj_rast <- terra::rast(
        nrows = terra::nrow(img),
        ncols = terra::ncol(img),
        xmin = terra::xmin(img),
        xmax = terra::xmax(img),
        ymin = terra::ymin(img),
        ymax = terra::ymax(img)
    )

    initial_seeds <- matrix(c(2L, 3L), ncol = 2, byrow = TRUE)
    colnames(initial_seeds) <- c("r", "c")

    click_valid_1 <- list(x = 1, y = 3)
    click_outside <- list(x = -1, y = 5)
    click_valid_2 <- list(x = 4, y = 1)
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

    seg_dummy <- structure(list(seg = img[[1L]]), class = "snic")
    captured <- list(
        snic = list(),
        plotRGB = list(),
        plot = list(),
        polygons = list()
    )

    valid_clicks <- list(click_valid_1, click_valid_2)
    prepared_initial <- as.data.frame(initial_seeds, stringsAsFactors = FALSE)
    additions <- do.call(
        rbind,
        lapply(valid_clicks, function(pt) {
            r <- terra::rowFromY(proj_rast, pt$y)
            c <- terra::colFromX(proj_rast, pt$x)
            data.frame(
                r = as.integer(r),
                c = as.integer(c),
                stringsAsFactors = FALSE
            )
        })
    )
    expected_final <- rbind(prepared_initial, additions)
    expected_rc <- expected_final[, c("r", "c")]

    captured <- list(snic = list())
    result <- with_mocked_bindings(
        {
            local_result <- NULL
            expect_message(
                {
                    local_result <- snic_grid_manual(
                        img,
                        seeds = initial_seeds,
                        snic_args = list(compactness = 0.25),
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
            snic = function(x, seeds, compactness, ...) {
                captured$snic[[length(captured$snic) + 1L]] <<- list(
                    x = x,
                    seeds = seeds,
                    compactness = compactness
                )
                seg_dummy
            },
            dev.interactive = function(...) TRUE,
            .package = "snic"
        ) %>%
        with_mocked_bindings(
            plotRGB = function(...) invisible(NULL),
            plot = function(...) invisible(NULL),
            as.polygons = function(x, ...) list(id = length(captured$snic)),
            .package = "terra"
        ) %>%
        with_mocked_bindings(
            interactive = function() TRUE,
            .package = "base"
        )

    expect_true(all(c("r", "c") %in% names(result)))
    expect_equal(result[, c("r", "c")], expected_rc)
    expect_equal(length(captured$snic), length(valid_clicks) + 1L)
    expect_true(
        all(vapply(
            captured$snic,
            function(call) identical(call$compactness, 0.25),
            logical(1)
        ))
    )
})
