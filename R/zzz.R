#' @useDynLib snic, .registration = TRUE
#' @importFrom graphics par locator
#' @importFrom stats quantile sd
NULL

.onLoad <- function(libname, pkgname) {
    # Load English messages
    .msg_load("en", .msg_en)
}
