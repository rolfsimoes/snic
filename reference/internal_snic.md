# Internal SNIC helpers

Developer-facing utilities wrapping the SNIC container metadata and the
native segmentation entry point.

## Usage

``` r
.snic_new(seg)

.snic_check(x)

.snic_seg(x)

.snic_means(x)

.snic_centroids(x)

.snic_animation(
  x,
  seeds,
  file_path,
  n_cycles,
  delay,
  progress,
  plot_args,
  snic_args,
  device_args
)

.snic_core(arr, seeds_rc, compactness)
```

## Functions

- `.snic_new(seg)` Packs the segmentation array and metadata into an S3
  container.

- `.snic_check(x)` Validates `snic` objects before accessing their
  slots.

- `.snic_seg(x)`, `.snic_means(x)`, `.snic_centroids(x)` Accessors
  returning the segmentation map, feature means, and centroids
  respectively.

- `.snic_animation(...)` Internal driver used by
  [`snic_animation()`](https://rolfsimoes.github.io/snic/reference/snic_animation.md)
  to orchestrate frame generation.

- `.snic_core(arr, seeds_rc, compactness)` Thin wrapper around the
  native `snic_snic` routine that performs the actual clustering.
