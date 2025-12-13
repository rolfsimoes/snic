# Interactive seed selection for SNIC segmentation

Collect seed points interactively by clicking on the image. Each
left-click adds a new seed; pressing `ESC` ends the session. After each
click, SNIC segmentation is recomputed and plotted for visual feedback.
This is intended for exploratory and fine-tuning workflows, where
automatic seeding may not be ideal.

## Usage

``` r
snic_grid_manual(
  x,
  seeds = NULL,
  ...,
  snic_args = list(compactness = 0.5),
  snic_plot_args = list(stretch = "lin", seeds_plot_args = list(pch = 4, col = "#FFFF00",
    cex = 1), seg_plot_args = list(border = "#FFFF00", col = NA, lwd = 0.4))
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object with a valid spatial reference and extent. Mouse clicks are
  interpreted in map coordinates.

- seeds:

  Optional existing seed set to display and extend. If pixel coordinates
  are supplied, they are internally converted. If `NULL`, the seed set
  is initialized empty and populated interactively.

- ...:

  Arguments forwarded to
  [`snic_plot`](https://rolfsimoes.github.io/snic/reference/snic_plot.md)
  for display control. These may include `band`, `r`, `g`, `b`,
  `stretch`, `seeds_plot_args`, or `seg_plot_args`.

- snic_args:

  A list of arguments passed to
  [`snic`](https://rolfsimoes.github.io/snic/reference/snic.md), such as
  `compactness`.

- snic_plot_args:

  A list of display modifiers forwarded to
  [`snic_plot`](https://rolfsimoes.github.io/snic/reference/snic_plot.md)
  when rendering the preview.

## Value

A two-column data frame of seed coordinates. If `x` lacks a CRS the
result is always pixel indices `(r, c)`. When `x` has a CRS:

- If `seeds` were supplied, their coordinate system is preserved in the
  output.

- Otherwise the result is expressed as `(lat, lon)` in `EPSG:4326`.

The output can be passed directly to
[`snic`](https://rolfsimoes.github.io/snic/reference/snic.md).

## Details

After each new seed is placed interactively, segmentation is recomputed
to provide immediate feedback on how the seed placement affects
clustering.

## See also

[`snic`](https://rolfsimoes.github.io/snic/reference/snic.md),
[`snic_grid`](https://rolfsimoes.github.io/snic/reference/snic_grid.md),
[`snic_animation`](https://rolfsimoes.github.io/snic/reference/snic_animation.md).

## Examples

``` r
if (interactive() && requireNamespace("terra", quietly = TRUE) && terra_is_working()) {
    tiff_dir <- system.file("demo-geotiff",
        package = "snic",
        mustWork = TRUE
    )
    files <- file.path(
        tiff_dir,
        c(
            "S2_20LMR_B02_20220630.tif",
            "S2_20LMR_B04_20220630.tif",
            "S2_20LMR_B08_20220630.tif",
            "S2_20LMR_B12_20220630.tif"
        )
    )
    s2 <- terra::aggregate(terra::rast(files), fact = 8)

    seeds <- snic_grid_manual(
        s2,
        snic_args = list(compactness = 0.1),
        snic_plot_args = list(r = 4, g = 3, b = 1)
    )

    seg <- snic(s2, seeds, compactness = 0.1)

    snic_plot(
        s2,
        r = 4, g = 3, b = 1,
        stretch = "lin",
        seeds = seeds,
        seg = seg
    )
}
```
