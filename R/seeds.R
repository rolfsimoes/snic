wgs84_to_xy <- function(x, seeds_wgs84) {
    UseMethod("wgs84_to_xy", x)
}

xy_to_rc <- function(x, seeds_xy) {
    UseMethod("xy_to_rc", x)
}

rc_to_wgs84 <- function(x, seeds_rc) {
    UseMethod("rc_to_wgs84", x)
}

.seeds <- function(...) {
    data.frame(..., stringsAsFactors = FALSE)
}
