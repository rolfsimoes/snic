#' @useDynLib snic, .registration = TRUE
#' @importFrom graphics par locator
#' @importFrom grDevices dev.interactive
NULL

#' Package initialization hook
#'
#' Registers the built-in English message catalog during package load.
#' @keywords internal
#' @name snic_init_hooks
#' @rdname snic_init_hooks
.onLoad <- function(libname, pkgname) {
    # Load English messages
    .msg_load("en", .msg_en)
}
