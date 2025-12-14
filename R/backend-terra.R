#' Check whether 'terra' CRS transformations are available
#'
#' This helper attempts the same coordinate reprojection used elsewhere in
#' the package (via \code{terra::project()}). When the bundled \code{PROJ} /
#' \code{GDAL} data are missing, \code{terra} raises an error; here we catch
#' it and return \code{FALSE} so that examples, tests, and vignettes can
#' skip gracefully.
#'
#' Results are cached for the current R session to avoid repeatedly hitting
#' \code{terra::project()} during a single run.
#'
#' @return A logical flag indicating whether \pkg{terra} can perform CRS
#'   transformations.
#' @export
terra_is_working <- local({
    cached <- NULL
    function() {
        if (!is.null(cached)) {
            return(cached)
        }
        if (!requireNamespace("terra", quietly = TRUE)) {
            cached <<- FALSE
            return(FALSE)
        }
        cached <<- tryCatch(
            {
                test_vect <- suppressWarnings(terra::vect(
                    data.frame(x = 0, y = 0),
                    geom = c("x", "y"),
                    crs = "EPSG:3857"
                ))
                suppressWarnings(terra::project(test_vect, "EPSG:4326"))
                TRUE
            },
            error = function(e) {
                FALSE
            }
        )
        cached
    }
})

#' @rdname snic_backends
#' @export
.check_x.SpatRaster <- function(x, param_name = "x") {
    if (!inherits(x, "SpatRaster")) {
        stop(.msg("terra_check_invalid_raster", param_name), call. = FALSE)
    }
    x
}

#' @rdname snic_backends
#' @export
.has_crs.SpatRaster <- function(x) {
    crs <- terra::crs(x)
    is.character(crs) && !is.na(crs) && nzchar(crs)
}

#' @rdname snic_backends
#' @export
.wgs84_to_xy.SpatRaster <- function(x, seeds_wgs84) {
    stopifnot(.seeds_type(seeds_wgs84) == "wgs84")
    if (!terra_is_working()) {
        stop(.msg("terra_projection_unavailable"), call. = FALSE)
    }
    v <- terra::vect(seeds_wgs84, geom = c("lon", "lat"), crs = "EPSG:4326")
    v <- terra::project(v, terra::crs(x))
    as.data.frame(terra::crds(v))
}

#' @rdname snic_backends
#' @export
.xy_to_wgs84.SpatRaster <- function(x, seeds_xy) {
    stopifnot(.seeds_type(seeds_xy) == "xy")
    if (!terra_is_working()) {
        stop(.msg("terra_projection_unavailable"), call. = FALSE)
    }
    v <- terra::vect(seeds_xy, geom = c("x", "y"), crs = terra::crs(x))
    v <- terra::project(v, "EPSG:4326")
    coords <- terra::crds(v)
    .seeds(lat = coords[, "y"], lon = coords[, "x"])
}

#' @rdname snic_backends
#' @export
.xy_to_rc.SpatRaster <- function(x, seeds_xy) {
    stopifnot(.seeds_type(seeds_xy) == "xy")
    row <- terra::rowFromY(x, seeds_xy$y)
    col <- terra::colFromX(x, seeds_xy$x)
    .seeds(r = row, c = col)
}

#' @rdname snic_backends
#' @export
.rc_to_xy.SpatRaster <- function(x, seeds_rc) {
    stopifnot(.seeds_type(seeds_rc) == "rc")
    x_coord <- terra::xFromCol(x, seeds_rc$c)
    y_coord <- terra::yFromRow(x, seeds_rc$r)
    .seeds(x = x_coord, y = y_coord)
}

#' @rdname snic_backends
#' @export
.x_to_arr.SpatRaster <- function(x) {
    arr <- terra::values(x, mat = FALSE)
    # set in-place and check size internally
    .set_dim(arr, dim(x)[c(2L, 1L, 3L)])
    aperm(arr, c(2L, 1L, 3L))
}

#' @rdname snic_backends
#' @export
.arr_to_x.SpatRaster <- function(x, arr, names = NULL) {
    arr <- .x_to_arr(arr)
    stopifnot(all(dim(x)[c(1L, 2L)] == dim(arr)[c(1L, 2L)]))
    arr <- aperm(arr, c(2L, 1L, 3L))
    n_bands <- dim(arr)[[3L]]
    # set in-place and check size internally
    .set_dim(arr, c(prod(dim(x)[c(1L, 2L)]), n_bands))
    x <- terra::rast(x, nlyrs = n_bands, vals = arr)
    if (is.character(names)) {
        names(x) <- names
    }
    x
}

#' @rdname snic_backends
.x_bbox.SpatRaster <- function(x) {
    c(terra::xmin(x), terra::xmax(x), terra::ymin(x), terra::ymax(x))
}

#' @rdname snic_backends
#' @export
.get_idx.SpatRaster <- function(x, idx) {
    if (is.numeric(idx)) {
        return(idx)
    }
    bands <- terra::names(x)
    return(match(idx, bands))
}
