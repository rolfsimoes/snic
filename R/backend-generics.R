#' Backend abstraction layer for SNIC data structures
#'
#' These generics define the minimal interface required for SNIC to operate on
#' different raster / array backends. Implementations must convert between:
#' \itemize{
#'   \item geographic coordinates (\code{lat, lon}),
#'   \item projected map coordinates (\code{x, y}), and
#'   \item pixel indices (\code{r, c}) in image space,
#' }
#' and must provide functions to translate external image objects to raw
#' numeric arrays (and back) for input to the SNIC core.
#'
#' The SNIC algorithm itself only works with:
#' \itemize{
#'   \item a numeric array \code{arr} with dimensions
#'      \code{(height, width, bands)}, and
#'   \item a two-column matrix/data frame \code{seeds_rc} giving pixel
#'      coordinates.
#' }
#' All spatial logic, projection handling, and raster I/O is delegated to these
#' interface methods.
#'
#' @section Required Methods for each backend:
#' \itemize{
#'   \item \code{check_x(x)}
#'   Validate that \code{x} is a supported input type. Return \code{TRUE}
#'   invisibly if supported, or throw an error with a helpful message if not.
#'   This is the entry point for SNIC algorithm compatibility.
#'
#'   \item \code{has_crs(x)}
#'   Return \code{TRUE} if \code{x} carries a spatial reference system. Used
#'   to decide whether seeds are interpreted as pixel coordinates or lat/lon.
#'
#'   \item \code{wgs84_to_xy(x, seeds_wgs84)}
#'   Convert \code{(lat, lon)} coordinates in \code{EPSG:4326} to projected
#'   map coordinates of \code{x}'s CRS.
#'
#'   \item \code{xy_to_rc(x, seeds_xy)}
#'   Convert projected \code{(x, y)} (map) coordinates to image pixel indices
#'   \code{(r, c)}. Output must be integer and 1-based.
#'
#'   \item \code{rc_to_wgs84(x, seeds_rc)}
#'   Inverse of the above: convert 1-based pixel indices to \code{(lat, lon)}.
#'   Used to return seeds or segmentation results in geographic form.
#'
#'   \item \code{x_to_arr(x)}
#'   Convert image \code{x} to a numeric array of shape
#'   \code{(height, width, bands)} in column-major order. No normalization,
#'   scale adjustments, or band selection should be performed here.
#'
#'   \item \code{arr_to_x(x, arr, names = NULL)}
#'   Wrap a \code{(height, width)} unlabeled array back into the native data
#'   type of \code{x}, preserving extent, CRS, resolution, and metadata where
#'   possible.
#' }
#'
#' @note
#' Backends may differ dramatically in how they internally represent
#' coordinates and storage layouts. The only requirement is that these
#' methods form a consistent round-trip:
#' \preformatted{
#'     lat/lon  <->  (x, y)  <->  (r, c)  <->  arr
#' }
#'
#' @keywords internal
#' @name snic_backends
#' @rdname snic_backends
NULL


#' @rdname snic_backends
check_x <- function(x, param_name = "x") {
    UseMethod("check_x", x)
}

#' @rdname snic_backends
has_crs <- function(x) {
    UseMethod("has_crs", x)
}

#' @rdname snic_backends
wgs84_to_xy <- function(x, seeds_wgs84) {
    UseMethod("wgs84_to_xy", x)
}

#' @rdname snic_backends
xy_to_wgs84 <- function(x, seeds_xy) {
    UseMethod("xy_to_wgs84", x)
}

#' @rdname snic_backends
xy_to_rc <- function(x, seeds_xy) {
    UseMethod("xy_to_rc", x)
}

#' @rdname snic_backends
rc_to_xy <- function(x, seeds_rc) {
    UseMethod("rc_to_xy", x)
}

#' @rdname snic_backends
x_to_arr <- function(x) {
    UseMethod("x_to_arr", x)
}

#' @rdname snic_backends
arr_to_x <- function(x, arr, names = NULL) {
    UseMethod("arr_to_x", x)
}

#' @rdname snic_backends
x_bbox <- function(x) {
    UseMethod("x_bbox", x)
}

#' @rdname snic_backends
get_idx <- function(x, idx) {
    UseMethod("get_idx", x)
}

#' @rdname snic_backends
rc_to_wgs84 <- function(x, seeds_rc) {
    seeds_xy <- rc_to_xy(x, seeds_rc)
    xy_to_wgs84(x, seeds_xy)
}

#' @rdname snic_backends
wgs84_to_rc <- function(x, seeds_wgs84) {
    seeds_xy <- wgs84_to_xy(x, seeds_wgs84)
    xy_to_rc(x, seeds_xy)
}

#' @rdname snic_backends
#' @export
check_x.default <- function(x, param_name = "x") {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
has_crs.default <- function(x) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
wgs84_to_xy.default <- function(x, seeds_wgs84) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
xy_to_wgs84.default <- function(x, seeds_xy) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
xy_to_rc.default <- function(x, seeds_xy) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
rc_to_xy.default <- function(x, seeds_rc) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
x_to_arr.default <- function(x) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
arr_to_x.default <- function(x, arr, names = NULL) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
x_bbox.default <- function(x) {
    stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
}

#' @rdname snic_backends
#' @export
get_idx.default <- function(x, idx) {
    stop(.msg("unsupported_input_type"), class(x)[1], call. = FALSE)
}
