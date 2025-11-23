# Contributing to snic

Thanks for helping improve snic! This guide summarizes the internal API
conventions so you can add features with confidence, especially around
backends, seed helpers, and the message system.

- Please prefer focused PRs and add brief rationale in commit messages.
- Run `R CMD check` (or `devtools::check()`) locally before opening a
  PR.
- Keep changes consistent with the existing style and file layout.

**Naming Conventions**

- Functions and objects use snake_case; S3 methods follow
  `generic.class`.
- Internal-only helpers start with a leading dot and live in
  `internal-*` files.
  - Examples: `R/internal-utils.R:12`, `R/internal-grid.R:19`.
- User-facing entry points live in `api-*` files (exported with roxygen
  `@export`).
  - Examples: `R/api-snic.R:1`, `R/api-plot.R:1`.
- Backend implementations live in `backend-*` files and are implemented
  as S3 methods.
  - Examples: `R/backend-generics.R:1`, `R/backend-array.R:1`,
    `R/backend-terra.R:1`.
- Message keys are lower_snake_case and defined in language-specific
  lists.
  - Example keys: `R/msg-en.R:6`.

**Where Backends Live**

Backends adapt external image types to the minimal interface that SNIC
needs. The generics are defined in `R/backend-generics.R` and documented
under
[`?snic_backends`](https://rolfsimoes.github.io/snic/reference/snic_backends.md).

Required S3 methods to implement for your class `Cls` (pick only those
that make sense for your data type):

- `.check_x.Cls(x, param_name = "x")` - validate the input type; return
  `x` invisibly or [`stop()`](https://rdrr.io/r/base/stop.html) with a
  friendly message. See `R/backend-array.R:2` and `R/backend-terra.R:2`.
- `.has_crs.Cls(x)` - return `TRUE` if the object carries a spatial
  reference. See `R/backend-terra.R:12`.
- Coordinate transforms:
  - `.wgs84_to_xy.Cls(x, seeds_wgs84)` and
    `.xy_to_wgs84.Cls(x, seeds_xy)` when CRS-aware.
  - `.xy_to_rc.Cls(x, seeds_xy)` and `.rc_to_xy.Cls(x, seeds_rc)` must
    round-trip pixel indices and map coordinates. See
    `R/backend-terra.R:24` and `R/backend-terra.R:36`.
- Array translation:
  - `.x_to_arr.Cls(x)` must return a numeric array
    `(height, width, bands)` in column-major order. See
    `R/backend-terra.R:48`.
  - `.arr_to_x.Cls(x, arr, names = NULL)` must wrap a single-band
    `(h, w)` array back into the native type, preserving
    extent/CRS/metadata. See `R/backend-terra.R:57`.
- `.x_bbox.Cls(x)` - return `c(xmin, xmax, ymin, ymax)` in the object’s
  CRS. See `R/backend-terra.R:74`.

Placement and scaffolding:

- Create a new file `R/backend-yourpkg.R` and implement the S3 methods
  above for your class (e.g., `RasterBrick`, `stars`, `magick-image`).
- Add roxygen tags `@rdname snic_backends` and `@export` to each method
  so documentation and NAMESPACE entries are generated together with the
  generics. See patterns in `R/backend-array.R:1` and
  `R/backend-terra.R:1`.
- Prefer reusing existing message keys from `R/msg-en.R` via
  [`.msg()`](https://rolfsimoes.github.io/snic/reference/message_helpers.md)
  for validation errors. Add new keys only when needed (see “Message
  helpers”).
- Keep conversions consistent: `lat/lon <-> (x,y) <-> (r,c) <-> arr`.
  See the round-trip helpers in `R/backend-generics.R:95`.

Minimal example skeleton for a spatial backend (pseudo-code):

``` r
# R/backend-stars.R
#' @rdname snic_backends
#' @export
.check_x.stars <- function(x, param_name = "x") {
  if (!inherits(x, "stars")) stop(.msg("unsupported_input_type", class(x)[1]), call. = FALSE)
  x
}

#' @rdname snic_backends
#' @export
.has_crs.stars <- function(x) {
  # return TRUE when CRS is present
}

#' @rdname snic_backends
#' @export
.wgs84_to_xy.stars <- function(x, seeds_wgs84) { /* ... */ }

#' @rdname snic_backends
#' @export
.xy_to_rc.stars <- function(x, seeds_xy) { /* ... */ }

#' @rdname snic_backends
#' @export
.rc_to_xy.stars <- function(x, seeds_rc) { /* ... */ }

#' @rdname snic_backends
#' @export
.x_to_arr.stars <- function(x) { /* return (h, w, bands) numeric array */ }

#' @rdname snic_backends
#' @export
.arr_to_x.stars <- function(x, arr, names = NULL) { /* wrap back into stars */ }

.x_bbox.stars <- function(x) { /* xmin, xmax, ymin, ymax */ }
```

Tips:

- For non-spatial data (plain arrays), it’s acceptable for
  `.wgs84_to_xy.*` and `.xy_to_wgs84.*` to
  `stop(.msg("array_no_projection_support"))`, as in
  `R/backend-array.R:17` and `R/backend-array.R:23`.
- `.x_to_arr.*` must not normalize or reorder bands - just expose raw
  numeric values.
- `.arr_to_x.*` should validate spatial dimensions and preserve metadata
  (names, CRS, extent) when possible.

**Seed Helpers**

Use the seed helpers to validate, convert, and build seed tables
consistently. All helpers return a data frame with standard column names
depending on the coordinate system:

- Pixel indices: `(r, c)`
- Map coordinates: `(x, y)`
- Geographic coordinates (WGS84): `(lat, lon)`

Key functions (see `R/seeds.R`):

- `.seeds_check(seeds, param_name = "seeds")` - validate data
  frame/matrix input and normalize columns; returns an empty `(r, c)`
  data frame for `NULL`. See `R/seeds.R:39` and `R/seeds.R:60`.
- `.seeds_type(seeds)` - returns one of `"rc"`, `"xy"`, `"wgs84"`. See
  `R/seeds.R:83`.
- `as_seeds_rc(seeds, x)`, `as_seeds_xy(seeds, x)`,
  `as_seeds_wgs84(seeds, x)` - convert between systems using backend
  methods. See `R/seeds.R:22`.
- `.seeds(...)` - small constructor that sets names and disables
  factors. See `R/seeds.R:117`.
- `.append_seed(seeds, new_seed)` - append a single row, enforcing
  matching coordinate type. See `R/seeds.R:105`.

Small examples:

``` r
# Validate and coerce
seeds <- .seeds_check(data.frame(r = c(10, 30), c = c(20, 40)))
stopifnot(.seeds_type(seeds) == "rc")

# Convert to map coordinates for a SpatRaster
seeds_xy <- as_seeds_xy(seeds, x)

# Create a single seed and append it
new_xy <- .seeds(x = 12.5, y = 34.0)
seeds_xy <- .append_seed(seeds_xy, new_xy)
```

**Message Helpers**

User-facing strings come from a lightweight message catalog. Use these
helpers from `R/msg.R` to keep errors and messages uniform and
translateable:

- `.msg(key, ..., lang = getOption("lang", "en"))` - render a message by
  key (optionally with `sprintf` arguments). See `R/msg.R:16`.
- `.msg_load(lang, msg_lst)` - register a language dictionary at load
  time. See `R/msg.R:10` and `R/zzz.R:12`.

Language dictionaries live in files like `R/msg-en.R` and define a named
list of message templates, e.g. `R/msg-en.R:6`.

When adding new messages:

- Choose a lower_snake_case key and add it to `R/msg-en.R`.
- Emit messages via `stop(.msg("your_new_key", args...), call. = FALSE)`
  or `message(.msg("..."))`.

**Development Workflow**

- Roxygen: run `devtools::document()` to regenerate `NAMESPACE` and Rd
  files after changing roxygen tags.
- Style: keep to base R + suggested packages already in use; follow
  patterns in nearby files rather than introducing new conventions.
- Tests: add focused tests under `tests/` when possible. Keep runtime
  short.
- C++ core: call into native code only via the tiny wrappers in
  `R/internal-utils.R` (e.g.,
  [`.snic_core()`](https://rolfsimoes.github.io/snic/reference/internal_snic.md),
  [`.set_dim()`](https://rolfsimoes.github.io/snic/reference/internal_utils.md)).
  Don’t change `.Call` sites ad-hoc.

Questions? Open an issue or start a draft PR and ask for early feedback.
