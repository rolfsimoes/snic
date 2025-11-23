# Internal utilities for native calls and helpers

Lightweight wrappers for calling into the C++ backend plus helper
utilities reused throughout the package.

## Usage

``` r
.call(fn_name, ...)

.expand(...)

.set_dim(x, dim)

.polygonize(x)

.rast_tmpl(x)

.modify_list(x, y)
```

## Functions

- `.call(fn_name, ...)` Executes the registered C++ symbol via `.Call`
  and wraps errors with friendly messages.

- `.expand(...)` A thin wrapper around
  [`expand.grid()`](https://rdrr.io/r/base/expand.grid.html) that drops
  row attributes and prevents factor coercion.

- `.set_dim(x, dim)` Re-shapes `x` by delegating to the `snic_set_dim`
  native routine.

- `.polygonize(x)` Converts a segmentation raster into polygons.

- `.rast_tmpl(x)` Builds an empty
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  template matching the array's footprint.

- `.modify_list(x, y)` Modifies x list using named entries from y
