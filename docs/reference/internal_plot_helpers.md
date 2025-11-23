# Internal plotting utilities (developer documentation)

Shared helpers for rendering arrays and raster objects via terra. These
functions are not exported; they encapsulate the common steps needed for
plotting SNIC images, seeds, and segments.

## Usage

``` r
.plot_core(x, plot_args)

.plot_seeds(seeds_xy, x, plot_args, add)

.plot_segments(x, plot_args, add)
```

## Functions

- `.plot_core(x, plot_args)` Handles the low-level plotting of either a
  single band or an RGB composite.

- `.plot_seeds(seeds_xy, x, plot_args, add)` Draws seed locations over
  an existing plot window.

- `.plot_segments(x, plot_args, add)` Converts segmentation rasters into
  polygons and plots them with the supplied style overrides.
