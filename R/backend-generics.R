#' Backend abstraction layer for SNIC data structures
#'
#' These generics define the minimal interface required for SNIC to operate on
#' different raster / array backends. Implementations must convert between:
#' \itemize{
#'   \item geographic coordinates \code{(lat, lon)},
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
#'   \item \code{.check_x(x)}
#'   Validate that \code{x} is a supported input type. Return the validated
#'   object (usually \code{x}) invisibly if supported, or throw an error with a
#'   helpful message if not. This is the entry point for SNIC algorithm
#'   compatibility.
#'
#'   \item \code{.has_crs(x)}
#'   Return \code{TRUE} if \code{x} carries a spatial reference system. Used
#'   to decide whether seeds are interpreted as pixel coordinates or
#'   \code{(lat, lon)}.
#'
#'   \item \code{.wgs84_to_xy(x, seeds_wgs84)}
#'   Convert \code{(lat, lon)} coordinates in \code{EPSG:4326} to projected
#'   map coordinates of \code{x}'s CRS.
#'
#'   \item \code{.xy_to_rc(x, seeds_xy)}
#'   Convert projected \code{(x, y)} (map) coordinates to image pixel indices
#'   \code{(r, c)}. Output must be integer and 1-based.
#'
#'   \item \code{.rc_to_wgs84(x, seeds_rc)}
#'   Inverse of the above: convert 1-based pixel indices to \code{(lat, lon)}.
#'   Used to return seeds or segmentation results in geographic form.
#'
#'   \item \code{.x_to_arr(x)}
#'   Convert image \code{x} to a numeric array of shape
#'   \code{(height, width, bands)} in column-major order. No normalization,
#'   scale adjustments, or band selection should be performed here.
#'
#'   \item \code{.arr_to_x(x, arr, names = NULL)}
#'   Wrap a \code{(height, width, bands)} numeric array (often single-band
#'   output from SNIC) back into the native data type of \code{x}, preserving
#'   extent, CRS, resolution, and metadata where possible.
#'
#'   \item \code{.xy_to_wgs84(x, seeds_xy)} and \code{.rc_to_xy(x, seeds_rc)}
#'   Inverse conversions that return geographic or projected map coordinates
#'   from the respective inputs.
#'
#'   \item \code{.x_bbox(x)} Return \code{c(xmin, xmax, ymin, ymax)} in the
#'   CRS (or pixel) coordinate system of \code{x}.
#'
#'   \item \code{.get_idx(x, idx)} Resolve character band names or numeric
#'   indices into explicit numeric positions for use in downstream helpers.
#' }
#'
#' @param x Backend-specific raster/array object that implements these
#'   conversions.
#' @param param_name Parameter name to echo in validation errors for
#'   \code{.check_x()}.
#' @param seeds_wgs84 Two-column seed object with columns \code{lat} and
#'   \code{lon} (\code{EPSG:4326}).
#' @param seeds_xy Two-column seed object with columns \code{x} and \code{y}
#'   expressed in the CRS of \code{x}.
#' @param seeds_rc Two-column seed object with 1-based pixel indices
#'   \code{r} (row) and \code{c} (column).
#' @param arr Numeric array with dimensions \code{(height, width, bands)}.
#' @param names Optional character vector of band names applied by
#'   \code{.arr_to_x()} when the target backend supports them.
#' @param idx Numeric or character band identifiers to resolve via
#'   \code{.get_idx()}.
#'
#' @return
#' Results depend on the generic:
#' \itemize{
#'   \item \code{.check_x()}: the validated backend object (usually \code{x})
#'   returned invisibly; errors if unsupported.
#'   \item \code{.has_crs()}: logical flag indicating whether \code{x} carries
#'   a CRS.
#'   \item \code{.wgs84_to_xy()}: seed data frame with columns \code{x} and
#'   \code{y} in the CRS of \code{x}.
#'   \item \code{.xy_to_wgs84()}: seed data frame with columns \code{lat} and
#'   \code{lon} in \code{EPSG:4326}.
#'   \item \code{.xy_to_rc()}: seed data frame with 1-based integer columns
#'   \code{r} and \code{c}; values outside the raster extent may be
#'   \code{NA_integer_}.
#'   \item \code{.rc_to_xy()}: seed data frame with columns \code{x} and
#'   \code{y} in the CRS of \code{x}, with coordinates set to \code{NA} when
#'   indices fall outside the extent.
#'   \item \code{.rc_to_wgs84()}: seed data frame with columns \code{lat} and
#'   \code{lon} obtained by composing \code{.rc_to_xy()} and
#'   \code{.xy_to_wgs84()}.
#'   \item \code{.wgs84_to_rc()}: seed data frame with columns \code{r} and
#'   \code{c} obtained by composing \code{.wgs84_to_xy()} and
#'   \code{.xy_to_rc()}.
#'   \item \code{.x_to_arr()}: numeric array shaped \code{(height, width,
#'   bands)} in column-major order.
#'   \item \code{.arr_to_x()}: object of the same backend type as \code{x}
#'   containing the supplied array data, with band names applied when
#'   provided.
#'   \item \code{.x_bbox()}: numeric vector \code{c(xmin, xmax, ymin, ymax)}
#'   in the coordinate system of \code{x}.
#'   \item \code{.get_idx()}: numeric vector of band indices resolved from
#'   numeric or name-based input.
#' }
#'
#' @note
#' Backends may differ dramatically in how they internally represent
#' coordinates and storage layouts. The only requirement is that these
#' methods form a consistent round-trip:
#' \code{
#'     lat/lon  <->  (x, y)  <->  (r, c)  <->  arr
#' }
#'
#' @keywords internal
#' @name snic_backends
NULL


#' @rdname snic_backends
.check_x <- function(x, param_name = "x") {
    UseMethod(".check_x", x)
}

#' @rdname snic_backends
.has_crs <- function(x) {
    UseMethod(".has_crs", x)
}

#' @rdname snic_backends
.wgs84_to_xy <- function(x, seeds_wgs84) {
    UseMethod(".wgs84_to_xy", x)
}

#' @rdname snic_backends
.xy_to_wgs84 <- function(x, seeds_xy) {
    UseMethod(".xy_to_wgs84", x)
}

#' @rdname snic_backends
.xy_to_rc <- function(x, seeds_xy) {
    UseMethod(".xy_to_rc", x)
}

#' @rdname snic_backends
.rc_to_xy <- function(x, seeds_rc) {
    UseMethod(".rc_to_xy", x)
}

#' @rdname snic_backends
.x_to_arr <- function(x) {
    UseMethod(".x_to_arr", x)
}

#' @rdname snic_backends
.arr_to_x <- function(x, arr, names = NULL) {
    UseMethod(".arr_to_x", x)
}

#' @rdname snic_backends
.x_bbox <- function(x) {
    UseMethod(".x_bbox", x)
}

#' @rdname snic_backends
.get_idx <- function(x, idx) {
    UseMethod(".get_idx", x)
}

#' @rdname snic_backends
.rc_to_wgs84 <- function(x, seeds_rc) {
    seeds_xy <- .rc_to_xy(x, seeds_rc)
    .xy_to_wgs84(x, seeds_xy)
}

#' @rdname snic_backends
.wgs84_to_rc <- function(x, seeds_wgs84) {
    seeds_xy <- .wgs84_to_xy(x, seeds_wgs84)
    .xy_to_rc(x, seeds_xy)
}
