# Animated visualization of SNIC seeding and segmentation

Generate an animated GIF illustrating how SNIC segmentation evolves as
seeds are progressively added. This function runs a sequence of SNIC
segmentations using incremental subsets of the provided seeds and
compiles the results into an animation.

## Usage

``` r
snic_animation(
  x,
  seeds,
  file_path,
  max_frames = 100L,
  delay = 10,
  progress = getOption("snic.progress", FALSE),
  ...,
  snic_args = list(compactness = 0.5),
  device_args = list(res = 96, bg = "white")
)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  representing the image to segment. Dimensions and coordinate reference
  are inferred automatically.

- seeds:

  A two-column object specifying seed coordinates. If `x` has a CRS, use
  columns `lat` and `lon` (in `EPSG:4326`); otherwise use pixel indices
  `(r, c)`. Typically created with
  [`snic_grid`](https://rolfsimoes.github.io/snic/reference/snic_grid.md)
  or interactively with
  [`snic_grid_manual`](https://rolfsimoes.github.io/snic/reference/snic_grid_manual.md).

- file_path:

  Path where the resulting GIF is saved. The file must not already exist
  and the parent directory must be writable.

- max_frames:

  Maximum number of frames to render. If there are more seeds than
  `max_frames`, only the first `max_frames` seeds are used.

- delay:

  Per-frame delay in centiseconds (1/100 s). Passed to
  [`magick::image_animate()`](https://docs.ropensci.org/magick/reference/animation.html).
  Default is 10 (0.1 s per frame).

- progress:

  Logical scalar; if `TRUE`, show the textual progress bar while
  generating frames.

- ...:

  Additional arguments forwarded to
  [`snic_plot`](https://rolfsimoes.github.io/snic/reference/snic_plot.md)
  when drawing each frame (e.g., RGB band indices or palette options).

- snic_args:

  Named list of extra arguments passed to
  [`snic`](https://rolfsimoes.github.io/snic/reference/snic.md) on every
  iteration (e.g., `compactness`). Arguments `x` and `seeds` are
  reserved and cannot be overridden.

- device_args:

  Named list of arguments passed to
  [`grDevices::png()`](https://rdrr.io/r/grDevices/png.html) when
  rendering frames. Defaults to `list(res = 96, bg = "white")`. Values
  such as `width`, `height`, and `filename` are managed automatically.

## Value

Invisibly, the file path of the generated GIF.

## Details

For each iteration, the function adds one seed to the current set and
re-runs [`snic`](https://rolfsimoes.github.io/snic/reference/snic.md).
The segmentation and seed locations are drawn using
[`snic_plot`](https://rolfsimoes.github.io/snic/reference/snic_plot.md),
saved as PNGs, and then combined into an animated GIF using the magick
package. This is intended for exploratory and didactic use to illustrate
the influence of seed placement and parameters such as `compactness`.

## See also

[`snic`](https://rolfsimoes.github.io/snic/reference/snic.md),
[`snic_plot`](https://rolfsimoes.github.io/snic/reference/snic_plot.md),
[`snic_grid`](https://rolfsimoes.github.io/snic/reference/snic_grid.md),
[`snic_grid_manual`](https://rolfsimoes.github.io/snic/reference/snic_grid_manual.md).

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE) &&
    requireNamespace("magick", quietly = TRUE)) {
    tif_dir <- system.file("demo-geotiff", package = "snic", mustWork = TRUE)
    files <- file.path(
        tif_dir,
        c(
            "S2_20LMR_B02_20220630.tif",
            "S2_20LMR_B04_20220630.tif",
            "S2_20LMR_B08_20220630.tif",
            "S2_20LMR_B12_20220630.tif"
        )
    )
    s2 <- terra::aggregate(terra::rast(files), fact = 8)

    set.seed(42)
    seeds <- snic_grid(s2, type = "random", spacing = 10L, padding = 0L)

    gif_file <- snic_animation(
        s2,
        seeds = seeds,
        file_path = tempfile("snic-demo", fileext = ".gif"),
        max_frames = 20L,
        snic_args = list(compactness = 0.1),
        r = 4, g = 3, b = 1,
        device_args = list(height = 192, width = 256)
    )
    gif_file
}
#> Saved animation to /tmp/RtmpOZPVJC/snic-demo1eacc75749.gif (2.0 s, 10.0 fps)
#> [1] "/tmp/RtmpOZPVJC/snic-demo1eacc75749.gif"
```
