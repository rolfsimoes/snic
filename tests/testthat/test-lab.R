test_that("rgb2xyz converts sRGB primaries to XYZ", {
    # sRGB primaries mapped to XYZ under D65 white point
    primaries <- matrix(c(
        1, 0, 0,
        0, 1, 0,
        0, 0, 1
    ), ncol = 3, byrow = TRUE)

    xyz <- snic::rgb2xyz(primaries, min_value = rep(0, 3), max_value = rep(1, 3))

    expected <- matrix(c(
        0.4124564, 0.2126729, 0.0193339,
        0.3575761, 0.7151522, 0.1191920,
        0.1804375, 0.0721750, 0.9503041
    ), ncol = 3, byrow = TRUE)

    expect_equal(xyz, expected, tolerance = 1e-6)
})

test_that("gamma_correction validates input shape and range", {
    expect_error(
        snic:::gamma_correction(1:3, min_value = rep(0, 3), max_value = rep(1, 3)),
        "Input must be a matrix"
    )
    bad_matrix <- matrix(1, ncol = 2)
    expect_error(
        snic:::gamma_correction(bad_matrix, min_value = rep(0, 3), max_value = rep(1, 3)),
        "Input must have at least 3 columns"
    )
    wide_matrix <- matrix(runif(20), ncol = 5)
    expect_warning(
        snic:::gamma_correction(wide_matrix, min_value = rep(0, 3), max_value = rep(1, 3)),
        "Input has more than 3 columns"
    )
    expect_error(
        snic:::gamma_correction(matrix(runif(9), ncol = 3),
            min_value = c(0, 0, 1), max_value = c(1, 1, 0.5)
        ),
        "Each element in 'min_value' must be strictly less"
    )
})

test_that("xyz2lab returns L*a*b* for D65 reference white", {
    white_point <- matrix(c(0.95047, 1.00000, 1.08883), ncol = 3)
    lab <- snic::xyz2lab(white_point)
    expect_equal(as.numeric(lab[, "L"]), 100, tolerance = 1e-6)
    expect_equal(as.numeric(lab[, "a"]), 0, tolerance = 1e-6)
    expect_equal(as.numeric(lab[, "b"]), 0, tolerance = 1e-6)
})

test_that("xyz2lab validates input shape", {
    expect_error(
        snic::xyz2lab(1:3),
        "Input must be a matrix"
    )
    not_enough <- matrix(runif(6), ncol = 2)
    expect_error(
        snic::xyz2lab(not_enough),
        "Input must have at least 3 columns"
    )
    wide_xyz <- matrix(runif(12), ncol = 4)
    expect_warning(
        snic::xyz2lab(wide_xyz),
        "Input has more than 3 columns"
    )
})

test_that("rgb2lab matches known values for pure red", {
    red_rgb <- matrix(c(1, 0, 0), ncol = 3)
    lab <- snic::rgb2lab(red_rgb, min_value = rep(0, 3), max_value = rep(1, 3))
    expect_equal(as.numeric(lab[, "L"]), 53.24079, tolerance = 1e-4)
    expect_equal(as.numeric(lab[, "a"]), 80.09246, tolerance = 1e-4)
    expect_equal(as.numeric(lab[, "b"]), 67.20320, tolerance = 1e-4)
})

test_that("rgb2lab forwards validation from gamma correction", {
    wide_rgb <- matrix(runif(12), ncol = 4)
    expect_warning(
        snic::rgb2lab(wide_rgb, min_value = rep(0, 3), max_value = rep(1, 3)),
        "Input has more than 3 columns"
    )
    too_few <- matrix(runif(4), ncol = 2)
    expect_error(
        snic::rgb2lab(too_few, min_value = rep(0, 3), max_value = rep(1, 3)),
        "Input must have at least 3 columns"
    )
})
