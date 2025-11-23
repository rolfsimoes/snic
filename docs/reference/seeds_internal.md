# Internal seed utilities

Low-level helpers shared across the seed conversion and validation
stack. These functions never touch disk or user I/O; they simply
standardize how seeds are checked, typed, and appended so that
downstream helpers can focus on the conversions.

## Usage

``` r
.seeds_check(seeds)

# S3 method for class 'data.frame'
.seeds_check(seeds)

# S3 method for class 'matrix'
.seeds_check(seeds)

# S3 method for class '`NULL`'
.seeds_check(seeds)

# Default S3 method
.seeds_check(seeds)

.seeds_type(seeds)

.switch_seeds(seeds, ...)

.append_seed(seeds, new_seed)

.seeds(...)
```

## Arguments

- seeds:

  A data frame, matrix, or `NULL` describing seeds.

- new_seed:

  Single-row object to append to an existing seed set.

## Components

- `.seeds_check()`:

  Generic plus methods that coerce arbitrary inputs into the required
  column pairs while emitting targeted errors.

- `.seeds_type()`:

  Identifies which coordinate signature a seed object currently follows
  so that dispatchers can choose the right path.

- `.switch_seeds()`:

  Wrapper around [`switch()`](https://rdrr.io/r/base/switch.html) that
  routes execution based on the detected type.

- `.append_seed()`:

  Appends a single seed row after verifying the incoming and existing
  coordinate systems match.

- `.seeds()`:

  Thin data.frame constructor with consistent defaults used throughout
  tests and helper code.
