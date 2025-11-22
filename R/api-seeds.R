#' Convert seed coordinates between raster index, map, and WGS84 systems
#'
#' These helpers make it easy to express an existing set of seed coordinates in
#' a different coordinate system. Seeds are accepted when they already contain
#' the target column pair and otherwise are projected using the provided raster.
#'
#' @param seeds A data frame, \code{tibble}, or matrix that contains one of the
#'   column pairs \code{(r, c)}, \code{(x, y)}, or \code{(lat, lon)}.
#' @param x A \code{\link[terra:SpatRaster-class]{SpatRaster}} or in-memory
#'   array that supplies the metadata needed to perform conversions. For array
#'   inputs, note that the internal coordinate system has its y-axis inverted
#'   (increasing upwards) compared to matrix indices (which increase downwards).
#'
#' @return A data frame with the target coordinate columns and any additional
#'   columns that were present in \code{seeds}.
#'
#' @section Target systems:
#' \describe{
#'   \item{\code{as_seeds_rc()}}{Ensures seeds are expressed in raster row /
#'   column indices \code{(r, c)}.}
#'   \item{\code{as_seeds_xy()}}{Ensures seeds use the raster CRS in map units
#'   \code{(x, y)}.}
#'   \item{\code{as_seeds_wgs84()}}{Ensures seeds are in geographic
#'   coordinates \code{(lat, lon)} in \code{EPSG:4326}.}
#' }
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'     # Load a test Sentinel-2 band
#'     s2_file <- system.file(
#'         "demo-geotiff/S2_20LMR_B04_20220630.tif",
#'         package = "snic"
#'     )
#'     s2_rast <- terra::rast(s2_file)
#'
#'     # Create some test coordinates in pixel space
#'     seeds_rc <- data.frame(r = c(10, 20, 30), c = c(15, 25, 35))
#'
#'     # Convert to map coordinates (x,y)
#'     seeds_xy <- as_seeds_xy(seeds_rc, s2_rast)
#'
#'     # Convert to geographic coordinates (lat,lon)
#'     seeds_wgs84 <- as_seeds_wgs84(seeds_rc, s2_rast)
#' }
#'
#' @name seeds_api
NULL

#' @rdname seeds_api
#' @export
as_seeds_rc <- function(seeds, x) {
    .switch_seeds(
        seeds,
        rc = seeds,
        wgs84 = .wgs84_to_rc(x, seeds),
        xy = .xy_to_rc(x, seeds)
    )
}

#' @rdname seeds_api
#' @export
as_seeds_xy <- function(seeds, x) {
    .switch_seeds(
        seeds,
        rc = .rc_to_xy(x, seeds),
        wgs84 = .wgs84_to_xy(x, seeds),
        xy = seeds
    )
}

#' @rdname seeds_api
#' @export
#' @note Only rasters with a defined coordinate reference system (CRS) can be
#'   transformed to WGS84 coordinates. If the input raster `x` lacks a CRS,
#'   attempting to convert to WGS84 will result in an error.
as_seeds_wgs84 <- function(seeds, x) {
    .switch_seeds(
        seeds,
        rc = .rc_to_wgs84(x, seeds),
        wgs84 = seeds,
        xy = .rc_to_wgs84(x, seeds)
    )
}
