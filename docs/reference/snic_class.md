# SNIC segmentation container

Objects returned by
[`snic`](https://rolfsimoes.github.io/snic/reference/snic.md) inherit
from the `snic` S3 class. They are lightweight containers bundling the
segmentation result together with per-cluster summaries produced by the
SNIC algorithm. Internally, a `snic` object is a named list with
components:

- `seg`:

  The segmentation map in the native type of the input (either a 3D
  integer `array` with dimensions `(height, width, 1)` or a single-layer
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)—matching
  the input given to
  [`snic()`](https://rolfsimoes.github.io/snic/reference/snic.md)).
  Values are superpixel labels (positive integers); `NA` marks pixels
  that were not assigned.

- `means`:

  Numeric matrix with one row per superpixel and one column per input
  band (column names preserved when available), giving the mean feature
  value of each cluster. May be `NULL` if the backend cannot retain
  these summaries.

- `centroids`:

  Numeric matrix with columns `r` and `c` giving the cluster centers in
  pixel coordinates (0-based indices used by the SNIC core). May be
  `NULL` when centroids are unavailable.

The list carries class `"snic"` to enable the accessors and print
methods below; the segmentation labels index the corresponding rows of
`means` and `centroids`.

## Usage

``` r
snic_get_means(x)

snic_get_centroids(x)

snic_get_seg(x)

# S3 method for class 'snic'
snic_get_means(x)

# S3 method for class 'snic'
snic_get_centroids(x)

# S3 method for class 'snic'
snic_get_seg(x)

# S3 method for class 'snic'
print(x, ...)
```

## Arguments

- x:

  A `snic` object, typically the result of a call to
  [`snic`](https://rolfsimoes.github.io/snic/reference/snic.md). It
  stores the segmentation map along with per-cluster summaries (means,
  centroids, and metadata) produced by the SNIC algorithm.

- ...:

  Additional arguments passed to or from methods. Currently unused, but
  included for compatibility with S3 method dispatch.

## Value

- `snic_get_means()`: Numeric matrix of per-cluster band means, or
  `NULL` when not available.

- `snic_get_centroids()`: Numeric matrix with columns `r` and `c` giving
  cluster centers in pixel coordinates (0-based), or `NULL` when not
  available.

- `snic_get_seg()`: Segmentation map in the native type of the input
  (`array` or `SpatRaster`) with integer labels and possible `NA` for
  unassigned pixels.

- `print.snic()`: Invisibly returns `x` after printing a human-readable
  summary.

## Accessors

- `snic_get_seg`: Retrieve the segmentation result.

- `snic_get_means`: Retrieve per-cluster feature means.

- `snic_get_centroids`: Retrieve per-cluster centroids.

## Methods

- [`snic_animation`](https://rolfsimoes.github.io/snic/reference/snic_animation.md):
  Animate the segmentation process.

- [`print`](https://rdrr.io/r/base/print.html): Print a summary of the
  segmentation result.

- [`plot`](https://rspatial.github.io/terra/reference/plot.html):
  Visualize the segmentation result.
