
# snic <a id="top"></a>

<!-- badges: start -->

[![Check](https://github.com/rolfsimoes/snic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rolfsimoes/snic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Efficient superpixel segmentation for multi-band imagery using the
Simple Non-Iterative Clustering (SNIC) algorithm. The package wraps a
C++ implementation with an ergonomic R interface, integrates with
`terra` for raster workflows, and provides helpers for seed placement,
plotting, and reproducibility.

## Installation

``` r
# install.packages("pak")
pak::pak("rolfsimoes/snic")

# alternatively with remotes
# remotes::install_github("rolfsimoes/snic")
```

The `terra` package is suggested for raster support and required for
most of the plotting utilities demonstrated below.

## Highlights

- Implements SNIC with a fast C++ core exposed to R
- Works with in-memory arrays or `terra::SpatRaster` objects
- Offers multiple seeding strategies via
  `snic_grid(type = c("rectangular", "diamond", "hexagonal", "random"))` and
  interactive placement via `snic_grid_manual()`
- Includes ready-to-plot utilities (`snic_plot()`) for quick inspection
  of inputs, seeds, and resulting segments
- Ships with a Sentinel-2 subset
  (`system.file("S2-20LMR", package = "snic")`) for reproducible
  examples and tests

## Requirements and example data

- **Raster support.** `terra (>= 1.7)` is suggested and is required for
  most raster examples below. In-memory `array` workflows can skip it,
  but you will lose the quick plotting helpers.
- **Animation support.** `magick` is optional and only needed for
  `snic_animation()`. The chunk is cached so missing the package merely
  skips the demo.
- **Development helpers.** The README uses `pkgload::load_all()` when
  building from source to avoid installing the package during
  development. Installed users can simply `library(snic)`.
- **Sample imagery.** The bundled Sentinel-2 subset
  (`system.file("S2-20LMR", package = "snic")`) contains four bands
  cropped to a small agricultural site and is released under the original
  Copernicus Sentinel data policy.

## Key functions at a glance

| Task | Function(s) | Notes |
| --- | --- | --- |
| Place seeds on a regular grid | `snic_grid(type = "rectangular" | "diamond" | "hexagonal")` | Control spacing and padding per dimension. |
| Explore irregular layouts | `snic_grid(type = "random")`, `snic_grid_manual()` | Use random jittering or interactively edit seeds. |
| Estimate expected superpixels | `snic_count_seeds()` | Quick diagnostic before running the algorithm. |
| Run segmentation | `snic()` | Accepts arrays or `SpatRaster` objects and returns labeled rasters. |
| Inspect results | `snic_plot()`, `snic_animation()` | Static overlays or GIF-based reviews for QA/QC. |

## Why SNIC?

SNIC produces compact superpixels in near-linear time and avoids the
iterative updates of SLIC-like algorithms. The `snic` package exposes
those speed benefits through:

- A C++ core that processes moderate Sentinel-2 tiles (thousands ×
  thousands of pixels × multiple bands) in a few seconds on a laptop.
- Native `terra` integration, so you keep CRS, extent, and metadata
  intact.
- Reproducible seeding helpers, which makes parameter sweeps easy to
  script and compare.

## Pipeline overview

The SNIC workflow is short and reproducible:

- **Step 1 – Seed placement.** Select or draw a grid of starting seeds
  that guide where the superpixels will grow. Grids can be generated
  automatically with `snic_grid()` (rectangular, diamond, hexagonal, or
  random layouts) or crafted interactively with `snic_grid_manual()`.
- **Step 2 – Segmentation.** Run `snic()` with the chosen seeds to grow
  superpixels and inspect the result with `snic_plot()` or the animated
  helper `snic_animation()`.

## Quick start

The example below demonstrates a typical SNIC workflow with the bundled
Sentinel-2 subset. The helper `load_snic()` keeps the chunk runnable both
when building the README locally (using `pkgload`) and when the released
package is already installed.

``` r
load_snic <- function() {
  if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
    pkg_name <- tryCatch(
      read.dcf("DESCRIPTION", fields = "Package"),
      error = function(e) NULL
    )
    if (!is.null(pkg_name) && identical(pkg_name[[1]], "snic")) {
      return(pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE))
    }
  }

  if (requireNamespace("snic", quietly = TRUE)) {
    exports <- tryCatch(
      getNamespaceExports("snic"),
      error = function(e) character()
    )
    required_exports <- c("snic_grid", "snic_grid_manual")
    if (all(required_exports %in% exports)) {
      return(library(snic))
    }
  }

  if (requireNamespace("pkgload", quietly = TRUE)) {
    return(pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE))
  }

  stop("Install the development version (via pkgload) or the released snic package to run this example.", call. = FALSE)
}

load_snic()
#> ℹ Loading snic

library(terra)
#> terra 1.8.70

# Sentinel-2 subset packaged with snic
data_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
bands <- c("B02", "B04", "B08", "B12")
paths <- file.path(
  data_dir,
  sprintf("S2_20LMR_%s_20220630.tif", bands)
)

s2 <- terra::rast(paths)

# Aggregate raster to speed up the example
s2_small <- terra::aggregate(s2, fact = 5)

# Seed generation and segmentation
spacing <- 8L
seeds <- snic_grid(s2_small, type = "rectangular", spacing = spacing, padding = 0L)
segments <- snic(s2_small, seeds = seeds, compactness = 0.1)

# Store for later sections
s2_demo <- s2_small
seeds_rect <- seeds
segments_rect <- segments

# Visualise RGB composite with superpixel boundaries
snic_plot(
  s2_small,
  r = 4, g = 3, b = 1,
  stretch = "lin",
  seg = segments,
  seg_plot_args = list(border = "#FFFF00", col = NA, lwd = 0.6)
)
```

![](README_files/figure-gfm/quick-start-1.png)<!-- -->

For quick inspections from the console you can also rely on
`snic_plot()`:

``` r
snic_plot(
  s2_small,
  r = 4, g = 3, b = 1,
  seeds = seeds,
  seg = segments
)
```

![](README_files/figure-gfm/snic-plot-1.png)<!-- -->

## Step 1 – Seed placement

Seed placement controls the number, shape, and location of the resulting
superpixels. The package ships with several grid generators, each
returning a two-column (`r`, `c`) matrix ready for `snic()`:

- `snic_grid(type = "rectangular")` – equally spaced seeds along rows
  and columns.
- `snic_grid(type = "diamond")` – staggered rows produce a diagonal
  pattern that better respects gradients.
- `snic_grid(type = "hexagonal")` – hexagonal tiling for more isotropic
  superpixels.
- `snic_grid(type = "random")` – jittered seeds when structure is
  irregular or prior knowledge is limited.

Use `snic_count_seeds()` to forecast how many superpixels a spacing will
produce before running the algorithm.

``` r
set.seed(42)
spacing_demo <- 25L

grid_types <- c("rectangular", "diamond", "hexagonal", "random")
seed_examples <- setNames(
  lapply(grid_types, function(tp) {
    snic_grid(s2_demo, type = tp, spacing = spacing_demo, padding = 0L)
  }),
  tools::toTitleCase(grid_types)
)

op <- par(mfrow = c(2, 2), mar = c(1.5, 1.5, 2, 1))

for (name in names(seed_examples)) {
  snic_plot(
    s2_demo,
    r = 4, g = 3, b = 1,
    stretch = "lin",
    seeds = seed_examples[[name]],
    seg_plot_args = NULL,
    seeds_plot_args = list(pch = 3, col = "#F6D55C", lwd = 2)
  )
  title(name)
}
```

<figure>
<img src="README_files/figure-gfm/seed-strategies-1.png"
alt="Seed placement strategies on the Sentinel-2 example (spacing = 25)." />
<figcaption aria-hidden="true">Seed placement strategies on the
Sentinel-2 example (spacing = 25).</figcaption>
</figure>

``` r

snic_count_seeds(s2_demo, spacing = spacing_demo)
#> [1] 48

par(op)
```

### Interactive placement

Automatic grids get you started quickly, but experts can refine seeds
interactively. `snic_grid_manual()` opens a plotting device where you
can add, move, or remove seeds on-the-fly and then feed the result
straight into `snic()`:

``` r
manual_seeds <- snic_grid_manual(
  s2_demo,
  base_seeds = seeds_rect,
  r = 4, g = 3, b = 1,
  stretch = "lin"
)

segments_manual <- snic(
  s2_demo,
  seeds = manual_seeds,
  compactness = 0.1
)
```

## Step 2 – SNIC segmentation

Once seeds are defined, pass them to `snic()` together with the imagery
and a `compactness` factor. The result is a labeled raster that can be
visualized alongside the seeds for validation.

``` r
snic_plot(
  s2_demo,
  r = 4, g = 3, b = 1,
  stretch = "lin",
  seeds = seeds_rect,
  seg = segments_rect,
  seg_plot_args = list(border = "#56B4E9", col = NA, lwd = 0.6)
)
```

![](README_files/figure-gfm/segmentation-plot-1.png)<!-- -->

## Animated seeding review

`snic_animation()` replays the seeding process, adding one seed per
frame, re-running `snic()`, and composing the frames into a GIF. Cache
the chunk so the animation is generated only once.

<figure>
<img src="README_files/figure-gfm/segmentation-animation.gif"
alt="Sequential SNIC segmentation as seeds are added (random grid, 20 frames)." />
<figcaption aria-hidden="true">Sequential SNIC segmentation as seeds are
added (random grid, 20 frames).</figcaption>
</figure>

## Contributing

Bug reports, feature requests, and pull requests are welcome in the
[issue tracker](https://github.com/rolfsimoes/snic/issues). When
proposing changes:

- Run `R CMD check` or `devtools::check()` to keep the package stable.
- Re-knit `README.Rmd` if you touch code chunks so plots stay in sync.
- Mention whether raster dependencies (`terra`, `magick`) were available
  when reproducing a bug, as it affects plotting and animation paths.
