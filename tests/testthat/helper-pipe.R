# Minimal pipe operator for test readability.
`%>%` <- function(lhs, rhs) {
    lhs_call <- substitute(lhs)
    rhs_call <- substitute(rhs)
    if (!is.call(rhs_call)) {
        stop("Right-hand side of pipe must be a function call")
    }
    expr <- as.call(c(rhs_call[[1L]], lhs_call, as.list(rhs_call[-1L])))
    eval(expr, parent.frame())
}
