# Internal grid utilities (developer documentation)

These functions implement the core logic for generating seed coordinates
used in SNIC grid-based seeding. They are not exported and should not be
called directly by users. All grid functions operate in pixel-index
space (row/column). CRS-aware conversion is handled elsewhere.

## Usage

``` r
.check_grid_args(x, spacing, padding)

.grid_count_seeds(x, spacing, padding)

.grid_rect(x, spacing, padding)

.grid_diamond(x, spacing, padding)

.grid_hex(x, spacing, padding)

.grid_random(x, spacing, padding)

.grid_manual(x, snic_args, plot_args)
```

## Functions

- `.check_grid_args(x, spacing, padding)` Validates input dimensions and
  parameters. Ensures:

  - `x` has positive height and width,

  - `spacing` is numeric of length 2 and greater than 1,

  - `padding` is numeric of length 2 and non-negative,

  - padding does not eliminate all valid placement area.

- `.grid_count_seeds(x, spacing, padding)` Computes the number of grid
  points in each dimension that fit within the interior region defined
  by `padding`.

- `.grid_rect(x, spacing, padding)` Generates a rectangular grid of seed
  positions evenly spaced across the available region.

- `.grid_diamond(x, spacing, padding)` Generates a rectangular grid and
  a second grid offset diagonally by half the spacing, producing a
  diamond pattern. Boundary checks ensure offset points remain valid.

- `.grid_hex(x, spacing, padding)` Similar to `.grid_diamond`, but
  applies axis-dependent spacing to approximate a hexagonal tiling
  geometry.

- `.grid_random(x, spacing, padding)` Places
  `prod(.grid_count_seeds(...))` uniformly sampled seed positions inside
  the padded region. Sampling is without replacement.

- `.grid_manual(x, snic_args, snic_plot_args)` Interactive seeding.
  Displays an image and iteratively updates seeds based on mouse clicks.
  Re-runs SNIC and re-plots after each update. Intended for exploratory
  inspection, not automated workflows.
