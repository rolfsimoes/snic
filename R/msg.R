#' Internal message dictionary
#'
#' Stores lightweight translations for user-facing messages.
#' Extend this list with new language codes (e.g., "pt", "es") as needed.
#' @keywords internal
#' @name message_helpers
#' @rdname message_helpers
.msg_env <- new.env(parent = emptyenv())

#' @rdname message_helpers
#' @param lang Language code to register.
#' @param msg_lst Named character vector/list of message templates.
#' @keywords internal
.msg_load <- function(lang, msg_lst) {
    assign(lang, msg_lst, envir = .msg_env)
}

#' @rdname message_helpers
#' @param key Message identifier to retrieve.
#' @param ... Optional \code{sprintf} arguments.
#' @param lang Language override; defaults to \code{getOption("lang", "en")}.
#' @keywords internal
.msg <- function(key, ..., lang = getOption("lang", "en")) {
    if (!lang %in% names(.msg_env)) lang <- "en"
    dict <- .msg_env[[lang]]
    msg <- if (!is.null(dict) && key %in% names(dict)) dict[[key]] else key
    if (!missing(...)) msg <- sprintf(msg, ...)
    msg
}
