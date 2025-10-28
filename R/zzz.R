#' @useDynLib snic, .registration = TRUE, .fixes = "C_"
#' @importFrom utils getFromNamespace
.onUnload <- function(libpath) {
    library.dynam.unload("snic", libpath)
}
