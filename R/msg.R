#' Internal message dictionary
#'
#' Stores lightweight translations for user-facing messages.
#' Extend this list with new language codes (e.g., "pt", "es") as needed.
#' @keywords internal
.msg_env <- new.env(parent = emptyenv())

.msg_load <- function(lang, msg_lst) {
    assign(lang, msg_lst, envir = .msg_env)
}

.msg <- function(key, ..., lang = getOption("lang", "en")) {
    if (!lang %in% names(.msg_env)) lang <- "en"
    dict <- .msg_env[[lang]]
    msg <- if (!is.null(dict) && key %in% names(dict)) dict[[key]] else key
    if (!missing(...)) msg <- sprintf(msg, ...)
    msg
}
