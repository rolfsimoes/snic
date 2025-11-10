#' @export
has_crs.array <- function(x) {
    FALSE
}

#' @export
has_crs.SpatRaster <- function(x) {
    crs <- terra::crs(x)
    is.character(crs) && !is.na(crs) && nzchar(crs)
}

#' @export
wgs84_to_xy.array <- function(x, seeds_wgs84) {
    stop("array images does not support spatial projection")
}

#' @export
wgs84_to_xy.SpatRaster <- function(x, seeds_wgs84) {
    v <- terra::vect(seeds_wgs84, geom = c("lon", "lat"), crs = "EPSG:4326")
    v <- terra::project(v, terra::crs(x))
    as.data.frame(terra::crds(v))
}

#' @export
xy_to_rc.array <- function(x, seeds_xy) {
    .seeds(r = seeds_xy$y, c = seeds_xy$x)
}

#' @export
xy_to_rc.SpatRaster <- function(x, seeds_xy) {
    .seeds(
        r = terra::rowFromY(x, seeds_xy$y),
        c = terra::colFromX(x, seeds_xy$x)
    )
}

#' @export
rc_to_wgs84.SpatRaster <- function(x, seeds_rc) {
    if (!has_crs(x)) {
        return(seeds_rc)
    }
    xy <- .seeds(
        x = terra::xFromCol(x, seeds_rc$c),
        y = terra::yFromRow(x, seeds_rc$r)
    )
    v <- terra::vect(xy, geom = c("x", "y"), crs = terra::crs(x))
    v <- terra::project(v, "EPSG:4326")
    coords <- terra::crds(v)
    .seeds(lat = coords[, "y"], lon = coords[, "x"])
}

append_seed <- function(seeds, new_seed) {
    if (!is.null(seeds)) {
        if (seeds_type(seeds) != seeds_type(new_seed)) {
            stop("trying to append seed of different type")
        }
    }
    rbind(seeds, new_seed)
}

#' @export
x_to_arr.array <- function(x) {
    stopifnot(is.array(x))
    x
}

#' @export
x_to_arr.SpatRaster <- function(x) {
    stopifnot(inherits(x, "SpatRaster"))
    arr <- terra::values(x, mat = FALSE)
    # set in-place and check size internally
    set_dim(arr, dim(x)[c(2, 1, 3)])
    aperm(arr, c(2, 1, 3))
}

#' @export
arr_to_x.array <- function(x, arr, names = NULL) {
    stopifnot(is.array(x), is.array(arr))
    if (is.character(names)) {
        colnames(arr) <- names
    }
    arr
}

#' @export
arr_to_x.SpatRaster <- function(x, arr, names = NULL) {
    stopifnot(inherits(x, "SpatRaster"), is.array(arr))
    stopifnot(all(dim(x)[c(1, 2)] == dim(arr)[c(1, 2)]))
    b <- dim(arr)[[3L]]
    arr <- aperm(arr, c(2, 1, 3))
    # set in-place and check size internally
    set_dim(arr, c(prod(dim(x)[c(1, 2)]), b))
    x <- terra::rast(x, nlyrs = b, vals = arr)
    if (is.character(names)) {
        names(x) <- names
    }
    x
}

arr_to_rast <- function(arr) {
    stopifnot(is.array(arr))
    dims <- dim(arr)
    h <- dims[1]
    w <- dims[2]
    b <- dims[3]

    tmpl <- terra::rast(
        nrows = h,
        ncols = w,
        nlyrs = b,
        xmin = 0,
        xmax = w,
        ymin = 0,
        ymax = h,
        crs = ""
    )

    arr_to_x(tmpl, arr)
}

seeds_type <- function(seeds) {
    if (all(c("r", "c") %in% colnames(seeds))) {
        "rc"
    } else if (all(c("lon", "lat") %in% colnames(seeds))) {
        "wgs84"
    } else {
        stop("argument 'seeds' must have columns (lon, lat) or (r, c)")
    }
}

is_valid_array <- function(x) {
    is.array(x) && is.numeric(x) && length(dim(x)) == 3
}

is_seeds <- function(seeds, type = c("rc", "wgs84")) {
    type <- match.arg(type)
    seeds_type(seeds) == type
}

snic_core <- function(arr, seeds_rc, compactness) {
    if (!is_valid_array(arr)) {
        stop(.msg("img_must_be_numeric_array_three_dimensions"))
    }
    if (is.integer(arr)) {
        storage.mode(arr) <- "double"
    }

    if (!is_seeds(seeds_rc, "rc")) {
        stop(.msg("seeds_invalid_type"))
    }
    if (!nrow(seeds_rc)) {
        stop(.msg("seeds_must_have_coordinates"))
    }
    seeds_rc <- as.matrix(round(seeds_rc))
    storage.mode(seeds_rc) <- "integer"

    compactness <- as.numeric(compactness)

    .call("snic_snic", arr, seeds_rc, compactness, "F")
}

set_dim <- function(x, dim) {
    .call("snic_set_dim", x, as.integer(dim))
}

polygonize <- function(x) {
    arr <- snic_to_array(x)

    .call("snic_polygonize", arr)
}
.polygonize <- function(x) {
    terra::as.polygons(x, dissolve = TRUE, na.rm = TRUE)
}
