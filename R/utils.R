check_positive_scalar <- function(x,
                                  name,
                                  type = c("integer", "numeric"),
                                  allow_zero = FALSE) {
    type <- match.arg(type)
    bound_label <- if (allow_zero) "non-negative" else "positive"
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
        x < 0 || (x == 0 && !allow_zero)) {
        stop(
            sprintf(
                "Argument '%s' must be a %s finite numeric scalar",
                name,
                bound_label
            ),
            call. = FALSE
        )
    }
    if (type == "integer") {
        return(as.integer(x))
    }
    as.numeric(x)
}
