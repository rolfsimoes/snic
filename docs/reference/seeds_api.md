# Convert seed coordinates between raster index, map, and WGS84 systems

These helpers make it easy to express an existing set of seed
coordinates in a different coordinate system. Seeds are accepted when
they already contain the target column pair and otherwise are projected
using the provided raster.

## Usage

``` r
as_seeds_rc(seeds, x)

as_seeds_xy(seeds, x)

as_seeds_wgs84(seeds, x)
```

## Arguments

- seeds:

  A data frame, `tibble`, or matrix that contains one of the column
  pairs `(r, c)`, `(x, y)`, or `(lat, lon)`.

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or in-memory array that supplies the metadata needed to perform
  conversions. For array inputs, note that the internal coordinate
  system has its y-axis inverted (increasing upwards) compared to matrix
  indices (which increase downwards).

## Value

A data frame with the target coordinate columns and any additional
columns that were present in `seeds`.

## Note

Only rasters with a defined coordinate reference system (CRS) can be
transformed to WGS84 coordinates. If the input raster \`x\` lacks a CRS,
attempting to convert to WGS84 will result in an error.

## Target systems

- `as_seeds_rc()`:

  Ensures seeds are expressed in raster row / column indices `(r, c)`.

- `as_seeds_xy()`:

  Ensures seeds use the raster CRS in map units `(x, y)`.

- `as_seeds_wgs84()`:

  Ensures seeds are in geographic coordinates `(lat, lon)` in
  `EPSG:4326`.

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
    # Load a test Sentinel-2 band
    s2_file <- system.file(
        "demo-geotiff/S2_20LMR_B04_20220630.tif",
        package = "snic"
    )
    s2_rast <- terra::rast(s2_file)

    # Create some test coordinates in pixel space
    seeds_rc <- data.frame(r = c(10, 20, 30), c = c(15, 25, 35))

    # Convert to map coordinates (x,y)
    seeds_xy <- as_seeds_xy(seeds_rc, s2_rast)

    # Convert to geographic coordinates (lat,lon)
    seeds_wgs84 <- as_seeds_wgs84(seeds_rc, s2_rast)
}
```
