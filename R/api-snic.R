#' Simple Non-Iterative Clustering (SNIC) segmentation
#'
#' Segment an image into superpixels using the SNIC algorithm. This function
#' wraps a C++ implementation operating on any number of spectral bands and
#' uses 4-neighbor (von Neumann) connectivity.
#'
#' @param x Image data. For the \code{array} method this must be a numeric array
#'   with dimensions \code{(height, width, bands)} in column-major order
#'   (R's native storage). For the
#'   \code{\link[terra:SpatRaster-class]{SpatRaster}} method (from \pkg{terra}),
#'   dimensions and band ordering are inferred automatically.
#'
#' @param seeds Initial seed coordinates. The required format depends on the
#'   spatial status of \code{x}:
#'   \itemize{
#'     \item If \code{x} has no CRS: a two-column data frame \code{(r, c)}
#'     giving 1-based pixel coordinates.
#'     \item If \code{x} has a CRS: a two-column data frame with columns
#'     \code{lat} and \code{lon} expressed in \code{EPSG:4326}. These are
#'     converted internally to pixel coordinates before segmentation.
#'   }
#'   Seeds define the starting cluster centers. They are usually generated with
#'   \code{\link{snic_grid}} helpers (e.g. rectangular, hexagonal or random),
#'   or placed interactively via \code{\link{snic_grid_manual}}.
#'
#' @param compactness Non-negative numeric value controlling the balance
#'   between feature similarity and spatial proximity (default = 0.5).
#'   Larger values produce more spatially compact superpixels.
#'
#' @param ... Currently unused; reserved for future extensions.
#'
#' @details
#' The algorithm performs clustering in a joint space that includes the image's
#' spectral dimensions and two spatial coordinates. Each seed initializes a
#' region, and pixels are assigned based on the SNIC distance metric combining
#' spectral similarity and spatial distance, weighted by \code{compactness}.
#'
#' @return
#' An object of class \code{snic} bundling the segmentation result together
#' with per-cluster summaries produced by the SNIC algorithm. The segmentation
#' result can be accessed using \code{\link{snic_get_seg}}. The per-cluster
#' summaries can be accessed using \code{\link{snic_get_means}} and
#' \code{\link{snic_get_centroids}}.
#'
#' @seealso
#' \code{\link{snic_grid}} for seed generation,
#' \code{\link{snic_grid_manual}} for interactive placement,
#' \code{\link{snic_plot}} for visualizing results.
#'
#' @examples
#' # Example 1: Geospatial raster
#' if (requireNamespace("terra", quietly = TRUE)) {
#'     path <- system.file("demo-geotiff", package = "snic", mustWork = TRUE)
#'     files <- file.path(
#'         path,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'
#'     # Downsample for speed (optional)
#'     s2 <- terra::aggregate(terra::rast(files), fact = 8)
#'
#'     # Generate a regular grid of seeds (lat/lon because CRS is present)
#'     seeds <- snic_grid(
#'         s2,
#'         type    = "rectangular",
#'         spacing = 10L,
#'         padding = 18L
#'     )
#'
#'     # Run segmentation
#'     seg <- snic(s2, seeds, compactness = 0.25)
#'
#'     # Visualize RGB composite with seeds and segment boundaries
#'     snic_plot(
#'         s2,
#'         r = 4, g = 3, b = 1,
#'         stretch = "lin",
#'         seeds = seeds,
#'         seg = seg
#'     )
#' }
#'
#' # Example 2: In-memory image (JPEG) + Lab transform
#' # Uses an example image shipped with the package (no terra needed)
#' if (requireNamespace("jpeg", quietly = TRUE)) {
#'     img_path <- system.file(
#'         "demo-jpeg/clownfish.jpeg",
#'         package = "snic",
#'         mustWork = TRUE
#'     )
#'     rgb <- jpeg::readJPEG(img_path) # h x w x 3 in [0, 1]
#'
#'     # Convert sRGB -> CIE Lab for perceptual clustering
#'     dims <- dim(rgb)
#'     dim(rgb) <- c(dims[1] * dims[2], dims[3])
#'     lab <- grDevices::convertColor(
#'         rgb,
#'         from = "sRGB",
#'         to = "Lab",
#'         scale.in = 1,
#'         scale.out = 1 / 255
#'     )
#'     dim(lab) <- dims
#'     dim(rgb) <- dims
#'
#'     # Seeds in pixel coordinates for array inputs
#'     seeds_rc <- snic_grid(lab, type = "hexagonal", spacing = 20L)
#'
#'     # Segment in Lab space and plot L channel with boundaries
#'     seg <- snic(lab, seeds_rc, compactness = 0.1)
#'
#'     snic_plot(
#'         rgb,
#'         r = 1L,
#'         g = 2L,
#'         b = 3L,
#'         seg = seg,
#'         seg_plot_args = list(
#'             border = "black"
#'         )
#'     )
#' }
#' @export
snic <- function(x, seeds, compactness = 0.5, ...) {
    x <- .check_x(x)
    seeds <- .seeds_check(seeds)

    arr <- .x_to_arr(x)

    arr <- .snic_core(arr, as_seeds_rc(seeds, x), compactness)

    seg <- .arr_to_x(x, arr, "snic")

    .snic_new(seg)
}

#' SNIC segmentation container
#'
#' Objects returned by \code{\link{snic}} inherit from the \code{snic}
#' S3 class. They are lightweight containers bundling the segmentation
#' result together with per-cluster summaries produced by the SNIC
#' algorithm. Internally, a \code{snic} object is a named list with
#' components:
#' \describe{
#'   \item{\code{seg}}{The segmentation map in the native type of the input
#'   (either a 3D integer \code{array} with dimensions
#'   \code{(height, width, 1)} or a single-layer
#'   \code{\link[terra:SpatRaster-class]{SpatRaster}}—matching the input
#'   given to \code{snic()}). Values are superpixel labels (positive integers);
#'   \code{NA} marks pixels that were not assigned.}
#'   \item{\code{means}}{Numeric matrix with one row per superpixel and one
#'   column per input band (column names preserved when available), giving the
#'   mean feature value of each cluster. May be \code{NULL} if the backend
#'   cannot retain these summaries.}
#'   \item{\code{centroids}}{Numeric matrix with columns \code{r} and \code{c}
#'   giving the cluster centers in pixel coordinates (0-based indices used by
#'   the SNIC core). May be \code{NULL} when centroids are unavailable.}
#' }
#' The list carries class \code{"snic"} to enable the accessors and print
#' methods below; the segmentation labels index the corresponding rows of
#' \code{means} and \code{centroids}.
#'
#' @section Accessors:
#' \itemize{
#'   \item \code{\link{snic_get_seg}}: Retrieve the segmentation result.
#'   \item \code{\link{snic_get_means}}: Retrieve per-cluster feature means.
#'   \item \code{\link{snic_get_centroids}}: Retrieve per-cluster centroids.
#' }
#'
#' @section Methods:
#' \itemize{
#'   \item \code{\link{snic_animation}}: Animate the segmentation process.
#'   \item \code{\link{print}}: Print a summary of the segmentation result.
#'   \item \code{\link{plot}}: Visualize the segmentation result.
#' }
#'
#' @param x A \code{snic} object, typically the result of a call to
#'   \code{\link{snic}}. It stores the segmentation map along with
#'   per-cluster summaries (means, centroids, and metadata) produced
#'   by the SNIC algorithm.
#'
#' @param ... Additional arguments passed to or from methods.
#'   Currently unused, but included for compatibility with
#'   S3 method dispatch.
#'
#' @return
#' \itemize{
#'   \item \code{snic_get_means()}: Numeric matrix of per-cluster band means,
#'   or \code{NULL} when not available.
#'   \item \code{snic_get_centroids()}: Numeric matrix with columns
#'   \code{r} and \code{c} giving cluster centers in pixel coordinates
#'   (0-based), or \code{NULL} when not available.
#'   \item \code{snic_get_seg()}: Segmentation map in the native type of the
#'   input (\code{array} or \code{SpatRaster}) with integer labels and
#'   possible \code{NA} for unassigned pixels.
#'   \item \code{print.snic()}: Invisibly returns \code{x} after printing a
#'   human-readable summary.
#' }
#'
#' @name snic_class
NULL

#' @rdname snic_class
#' @export
snic_get_means <- function(x) {
    UseMethod("snic_get_means", x)
}

#' @rdname snic_class
#' @export
snic_get_centroids <- function(x) {
    UseMethod("snic_get_centroids", x)
}

#' @rdname snic_class
#' @export
snic_get_seg <- function(x) {
    UseMethod("snic_get_seg", x)
}

#' @rdname snic_class
#' @export
snic_get_means.snic <- function(x) {
    x <- .snic_check(x)
    .snic_means(x)
}

#' @rdname snic_class
#' @export
snic_get_centroids.snic <- function(x) {
    x <- .snic_check(x)
    .snic_centroids(x)
}

#' @rdname snic_class
#' @export
snic_get_seg.snic <- function(x) {
    x <- .snic_check(x)
    .snic_seg(x)
}

#' @rdname snic_class
#' @export
print.snic <- function(x, ...) {
    x <- .snic_check(x)
    seg <- .snic_seg(x)
    dims <- dim(seg)

    cat("SNIC segmentation\n")
    cat("  Size (rows x cols):  ", dims[1L], " x ", dims[2L], "\n", sep = "")

    means <- .snic_means(x)
    if (!is.null(means)) {
        cat("  Clusters:    ", nrow(means), "\n", sep = "")
        cat("  Features:    ", ncol(means), "\n", sep = "")
        features <- dimnames(seg)[[3L]]
        if (!is.null(features)) {
            cat("        (", paste(features, collapse = ", "), ")\n", sep = "")
        }
    }

    cat("Viewing first rows and columns:\n")
    rows <- min(6L, dims[[1L]])
    cols <- min(6, dims[[2L]])
    print(seg[seq_len(rows), seq_len(cols), 1L, drop = FALSE])
    invisible(x)
}


#' Animated visualization of SNIC seeding and segmentation
#'
#' Generate an animated GIF illustrating how SNIC segmentation evolves
#' as seeds are progressively added. This function runs a sequence of
#' SNIC segmentations using incremental subsets of the provided seeds
#' and compiles the results into an animation.
#'
#' @param x A \code{\link[terra:SpatRaster-class]{SpatRaster}}
#'   representing the image to segment. Dimensions and coordinate reference
#'   are inferred automatically.
#' @param seeds A two-column object specifying seed coordinates. If \code{x}
#'   has a CRS, use columns \code{lat} and \code{lon} (in \code{EPSG:4326});
#'   otherwise use pixel indices \code{(r, c)}. Typically created with
#'   \code{\link{snic_grid}} or interactively with
#'   \code{\link{snic_grid_manual}}.
#' @param file_path Path where the resulting GIF is saved. The file must not
#'   already exist and the parent directory must be writable.
#' @param max_frames Maximum number of frames to render. If there are more
#'   seeds than \code{max_frames}, only the first \code{max_frames} seeds are
#'   used.
#' @param delay Per-frame delay in centiseconds (1/100 s). Passed to
#'   \code{magick::image_animate()}. Default is 10 (0.1 s per frame).
#' @param progress Logical scalar; if \code{TRUE}, show the textual progress
#'   bar while generating frames.
#' @param ... Additional arguments forwarded to \code{\link{snic_plot}} when
#'   drawing each frame (e.g., RGB band indices or palette options).
#' @param snic_args Named list of extra arguments passed to \code{\link{snic}}
#'   on every iteration (e.g., \code{compactness}). Arguments \code{x} and
#'   \code{seeds} are reserved and cannot be overridden.
#' @param device_args Named list of arguments passed to
#'   \code{grDevices::png()} when rendering frames. Defaults to
#'   \code{list(res = 96, bg = "white")}. Values such as \code{width},
#'   \code{height}, and \code{filename} are managed automatically.
#'
#' @details
#' For each iteration, the function adds one seed to the current set and
#' re-runs \code{\link{snic}}. The segmentation and seed locations are drawn
#' using \code{\link{snic_plot}}, saved as PNGs, and then combined into an
#' animated GIF using the \pkg{magick} package. This is intended for
#' exploratory and didactic use to illustrate the influence of seed placement
#' and parameters such as \code{compactness}.
#'
#' @return Invisibly, the file path of the generated GIF.
#'
#' @seealso \code{\link{snic}}, \code{\link{snic_plot}},
#'   \code{\link{snic_grid}}, \code{\link{snic_grid_manual}}.
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE) &&
#'     requireNamespace("magick", quietly = TRUE)) {
#'     tif_dir <- system.file("demo-geotiff", package = "snic", mustWork = TRUE)
#'     files <- file.path(
#'         tif_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'     s2 <- terra::aggregate(terra::rast(files), fact = 8)
#'
#'     set.seed(42)
#'     seeds <- snic_grid(s2, type = "random", spacing = 10L, padding = 0L)
#'
#'     gif_file <- snic_animation(
#'         s2,
#'         seeds = seeds,
#'         file_path = tempfile("snic-demo", fileext = ".gif"),
#'         max_frames = 20L,
#'         snic_args = list(compactness = 0.1),
#'         r = 4, g = 3, b = 1,
#'         device_args = list(height = 192, width = 256)
#'     )
#'     gif_file
#' }
#' @export
snic_animation <- function(x,
                           seeds,
                           file_path,
                           max_frames = 100L,
                           delay = 10,
                           progress = getOption("snic.progress", FALSE),
                           ...,
                           snic_args = list(
                               compactness = 0.5
                           ),
                           device_args = list(
                               res = 96,
                               bg = "white"
                           )) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(.msg("terra_required"), call. = FALSE)
    }
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop(.msg("magick_required"), call. = FALSE)
    }

    x <- .check_x(x)
    seeds <- .seeds_check(seeds)

    if (!nrow(seeds)) {
        stop(.msg("seeds_cannot_be_null"), call. = FALSE)
    }

    if (missing(file_path)) {
        stop(.msg("file_path_required"), call. = FALSE)
    }
    if (is.null(file_path) || !is.character(file_path) ||
        length(file_path) != 1L || is.na(file_path) || !nzchar(file_path)) {
        stop(.msg("file_path_single_path"), call. = FALSE)
    }
    file_path <- path.expand(file_path)
    file_path <- normalizePath(file_path, mustWork = FALSE)
    if (file.exists(file_path)) {
        stop(.msg("animation_file_exists", file_path), call. = FALSE)
    }

    parent_dir <- dirname(file_path)
    if (!dir.exists(parent_dir)) {
        dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(parent_dir)) {
        stop(.msg("animation_dir_create_failed", parent_dir), call. = FALSE)
    }

    if (!is.numeric(max_frames) && max_frames < 1L) {
        stop(.msg("max_frames_positive_integer"), call. = FALSE)
    }
    max_frames <- as.integer(max_frames)
    n_cycles <- min(nrow(seeds), max_frames)

    if (!is.numeric(delay) || length(delay) != 1L ||
        !is.finite(delay) || delay <= 0) {
        stop(.msg("delay_positive_number"), call. = FALSE)
    }
    delay <- as.numeric(delay)

    if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
        stop(.msg("progress_must_be_logical"), call. = FALSE)
    }


    if (!is.list(snic_args)) {
        stop(.msg("snic_args_must_be_list"), call. = FALSE)
    }

    plot_args <- list(...)

    .snic_animation(
        x = x,
        seeds = seeds,
        file_path = file_path,
        n_cycles = n_cycles,
        delay = delay,
        progress = progress,
        plot_args = plot_args,
        snic_args = snic_args,
        device_args = device_args
    )
}
