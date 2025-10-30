.colmaj <- function(x, h, w, b) {
    .Call(
        C_snic_colmaj,
        x,
        as.integer(h),
        as.integer(w),
        as.integer(b)
    )
}
