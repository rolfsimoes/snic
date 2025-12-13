skip_if_terra_broken <- function() {
    skip_if_not_installed("terra")
    if (!terra_is_working()) {
        skip("terra CRS transformations unavailable")
    }
}
