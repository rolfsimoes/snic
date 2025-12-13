# Check whether 'terra' CRS transformations are available

This helper attempts the same coordinate reprojection used elsewhere in
the package (via
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html)).
When the bundled `PROJ` / `GDAL` data are missing, `terra` raises an
error; here we catch it and return `FALSE` so that examples, tests, and
vignettes can skip gracefully.

## Usage

``` r
terra_is_working()
```

## Value

A logical flag indicating whether terra can perform CRS transformations.

## Details

Results are cached for the current R session to avoid repeatedly hitting
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
during a single run.
