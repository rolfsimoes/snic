# SNIC segmentation container

Objects returned by
[`snic`](https://rolfsimoes.github.io/snic/reference/snic.md) inherit
from the `snic` S3 class. They are lightweight containers bundling the
segmentation result together with per-cluster summaries produced by the
SNIC algorithm.

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

## Accessors

`snic_get_seg`: Retrieve the segmentation result. `snic_get_means`:
Retrieve per-cluster feature means. `snic_get_centroids`: Retrieve
per-cluster centroids.

## Methods

[`snic_animation`](https://rolfsimoes.github.io/snic/reference/snic_animation.md):
Animate the segmentation process.
[`print`](https://rdrr.io/r/base/print.html): Print a summary of the
segmentation result.
[`plot`](https://rspatial.github.io/terra/reference/plot.html):
Visualize the segmentation result.
