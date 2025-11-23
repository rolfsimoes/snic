# Backend abstraction layer for SNIC data structures

These generics define the minimal interface required for SNIC to operate
on different raster / array backends. Implementations must convert
between:

- geographic coordinates `(lat, lon)`,

- projected map coordinates (`x, y`), and

- pixel indices (`r, c`) in image space,

and must provide functions to translate external image objects to raw
numeric arrays (and back) for input to the SNIC core.

## Usage

``` r
# S3 method for class 'array'
.check_x(x, param_name = "x")

# S3 method for class 'array'
.has_crs(x)

# S3 method for class 'array'
.wgs84_to_xy(x, seeds_wgs84)

# S3 method for class 'array'
.xy_to_wgs84(x, seeds_xy)

# S3 method for class 'array'
.xy_to_rc(x, seeds_xy)

# S3 method for class 'array'
.rc_to_xy(x, seeds_rc)

# S3 method for class 'array'
.x_to_arr(x)

# S3 method for class 'array'
.arr_to_x(x, arr, names = NULL)

# S3 method for class 'array'
.x_bbox(x)

# S3 method for class 'array'
.get_idx(x, idx)

.check_x(x, param_name = "x")

.has_crs(x)

.wgs84_to_xy(x, seeds_wgs84)

.xy_to_wgs84(x, seeds_xy)

.xy_to_rc(x, seeds_xy)

.rc_to_xy(x, seeds_rc)

.x_to_arr(x)

.arr_to_x(x, arr, names = NULL)

.x_bbox(x)

.get_idx(x, idx)

.rc_to_wgs84(x, seeds_rc)

.wgs84_to_rc(x, seeds_wgs84)

# S3 method for class 'SpatRaster'
.check_x(x, param_name = "x")

# S3 method for class 'SpatRaster'
.has_crs(x)

# S3 method for class 'SpatRaster'
.wgs84_to_xy(x, seeds_wgs84)

# S3 method for class 'SpatRaster'
.xy_to_wgs84(x, seeds_xy)

# S3 method for class 'SpatRaster'
.xy_to_rc(x, seeds_xy)

# S3 method for class 'SpatRaster'
.rc_to_xy(x, seeds_rc)

# S3 method for class 'SpatRaster'
.x_to_arr(x)

# S3 method for class 'SpatRaster'
.arr_to_x(x, arr, names = NULL)

# S3 method for class 'SpatRaster'
.x_bbox(x)

# S3 method for class 'SpatRaster'
.get_idx(x, idx)
```

## Details

The SNIC algorithm itself only works with:

- a numeric array `arr` with dimensions `(height, width, bands)`, and

- a two-column matrix/data frame `seeds_rc` giving pixel coordinates.

All spatial logic, projection handling, and raster I/O is delegated to
these interface methods.

## Note

Backends may differ dramatically in how they internally represent
coordinates and storage layouts. The only requirement is that these
methods form a consistent round-trip:
` lat/lon <-> (x, y) <-> (r, c) <-> arr `

## Required Methods for each backend

- `.check_x(x)` Validate that `x` is a supported input type. Return
  `TRUE` invisibly if supported, or throw an error with a helpful
  message if not. This is the entry point for SNIC algorithm
  compatibility.

- `.has_crs(x)` Return `TRUE` if `x` carries a spatial reference system.
  Used to decide whether seeds are interpreted as pixel coordinates or
  `(lat, lon)`.

- `.wgs84_to_xy(x, seeds_wgs84)` Convert `(lat, lon)` coordinates in
  `EPSG:4326` to projected map coordinates of `x`'s CRS.

- `.xy_to_rc(x, seeds_xy)` Convert projected `(x, y)` (map) coordinates
  to image pixel indices `(r, c)`. Output must be integer and 1-based.

- `.rc_to_wgs84(x, seeds_rc)` Inverse of the above: convert 1-based
  pixel indices to `(lat, lon)`. Used to return seeds or segmentation
  results in geographic form.

- `.x_to_arr(x)` Convert image `x` to a numeric array of shape
  `(height, width, bands)` in column-major order. No normalization,
  scale adjustments, or band selection should be performed here.

- `.arr_to_x(x, arr, names = NULL)` Wrap a `(height, width)` unlabeled
  array back into the native data type of `x`, preserving extent, CRS,
  resolution, and metadata where possible.
