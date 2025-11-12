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
#' A single-band object with the same spatial dimensions as \code{x}, where
#' each pixel value is the integer label of its assigned superpixel.
#'
#' @seealso
#' \code{\link{snic_grid}} for seed generation,
#' \code{\link{snic_grid_manual}} for interactive placement,
#' \code{\link{snic_count_seeds}} for estimating seed counts,
#' \code{\link{snic_plot}} for visualizing results.
#'
#' @examples
#' # Example 1: Terra raster workflow with visual feedback
#' if (requireNamespace("terra", quietly = TRUE)) {
#'     tdir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     files <- file.path(
#'         tdir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'
#'     # Downsample for speed (optional)
#'     s2 <- terra::aggregate(terra::rast(files), fact = 5)
#'
#'     # Generate a regular grid of seeds (lat/lon because CRS is present)
#'     seeds <- snic_grid(
#'         s2,
#'         type    = "rectangular",
#'         spacing = 10L,
#'         padding = 50L
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
#'     img_path <- system.file("clownfish.jpeg", package = "snic", mustWork = TRUE)
#'     rgb <- jpeg::readJPEG(img_path) # h x w x 3 in [0, 1]
#'
#'     # Convert sRGB -> CIE Lab for perceptual clustering
#'     dims <- dim(rgb)
#'     dim(rgb) <- c(dims[1] * dims[2], dims[3])
#'     lab <- grDevices::convertColor(rgb, from = "sRGB", to = "Lab", scale.in = 1, scale.out = 1 / 255)
#'     dim(lab) <- dims
#'     dim(rgb) <- dims
#'
#'     # Seeds in pixel coordinates for array inputs
#'     seeds_rc <- snic_grid(lab, type = "hexagonal", spacing = 20L, padding = 4L)
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
    x <- check_x(x)
    seeds <- check_seeds(seeds)

    arr <- x_to_arr(x)

    arr <- snic_core(arr, as_seeds_rc(seeds, x), compactness)

    arr_to_x(x, arr, "snic")
}

#' Animated visualization of SNIC seeding and segmentation
#'
#' Generate an animated GIF illustrating how SNIC segmentation evolves
#' as seeds are progressively added. This function runs a sequence of
#' SNIC segmentations using incremental subsets of the provided seeds
#' and compiles the results into an animation.
#'
#' @param x A \code{\link[terra:SpatRaster-class]{terra::SpatRaster}}
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
#' @param ... Additional arguments forwarded to \code{\link{snic_plot}} when
#'   drawing each frame (e.g., RGB band indices or palette options).
#' @param snic_args Named list of extra arguments passed to \code{\link{snic}}
#'   on every iteration (e.g., \code{compactness}). Arguments \code{x} and
#'   \code{seeds} are reserved and cannot be overridden.
#' @param plot_args Reserved for future use. Currently ignored.
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
#'     tif_dir <- system.file("S2-20LMR", package = "snic", mustWork = TRUE)
#'     band_files <- file.path(
#'         tif_dir,
#'         c(
#'             "S2_20LMR_B02_20220630.tif",
#'             "S2_20LMR_B04_20220630.tif",
#'             "S2_20LMR_B08_20220630.tif",
#'             "S2_20LMR_B12_20220630.tif"
#'         )
#'     )
#'     s2 <- terra::aggregate(terra::rast(band_files), fact = 5)
#'
#'     set.seed(42)
#'     seeds <- snic_grid(s2, type = "random", spacing = 10L, padding = 0L)
#'
#'     gif_file <- snic_animation(
#'         s2,
#'         seeds = seeds,
#'         file_path = tempfile("snic-demo", fileext = ".gif"),
#'         max_frames = 150L,
#'         snic_args = list(compactness = 0.1),
#'         r = 4, g = 3, b = 1
#'     )
#'     gif_file
#' }
#' @export
snic_animation <- function(x,
                           seeds,
                           file_path,
                           max_frames = 100L,
                           delay = 10,
                           ...,
                           snic_args = list(
                               compactness = 0.5
                           ),
                           plot_args = list(),
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

    x <- check_x(x)
    seeds <- check_seeds(seeds)

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
    file_path <- normalizePath(path.expand(file_path), mustWork = FALSE)
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


    if (!is.list(snic_args)) {
        stop(.msg("snic_args_must_be_list"), call. = FALSE)
    }

    # temp files for frames
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    tmp_name <- sprintf(".snic-seeding-%s-%d", timestamp, Sys.getpid())
    frame_dir <- file.path(tempdir(), tmp_name)
    if (!dir.create(frame_dir, recursive = TRUE, showWarnings = FALSE) &&
        !dir.exists(frame_dir)) {
        stop(.msg("animation_dir_create_failed", frame_dir), call. = FALSE)
    }
    on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)

    dims <- dim(x)
    h <- dims[[1]]
    w <- dims[[2]]

    pb <- utils::txtProgressBar(max = n_cycles + 10L, style = 3)
    frame_files <- character(n_cycles)
    for (i in seq_len(n_cycles)) {
        utils::setTxtProgressBar(pb, i)
        current_seeds <- seeds[seq_len(i), , drop = FALSE]
        seg <- do.call(snic, c(list(x = x, seeds = current_seeds), snic_args))

        frame_file <- file.path(frame_dir, sprintf("frame-%02d.png", i))
        frame_files[[i]] <- frame_file

        default_device_args <- list(height = h, width = w)
        device_args <- utils::modifyList(default_device_args, device_args)
        device_args$filename <- frame_file
        do.call(grDevices::png, device_args)
        tryCatch(
            {
                snic_plot(x, seeds = current_seeds, seg = seg, ...)
            },
            error = function(err) {
                grDevices::dev.off()
                stop(err)
            }
        )
        grDevices::dev.off()
    }

    total_duration <- (n_cycles * delay) / 100
    fps <- 100 / delay

    animation <- magick::image_read(frame_files)
    animation <- magick::image_coalesce(animation)
    # TODO: implement a method to reduce image
    # size like animation <- magick::image_quantize(
    # animation, max = 64, dither = TRUE)
    animation <- magick::image_animate(
        animation,
        delay = delay,
        dispose = "previous",
        optimize = TRUE
    )
    magick::image_write(
        animation,
        path = file_path,
        format = "gif",
        compression = "LZW"
    )
    utils::setTxtProgressBar(pb, n_cycles + 10L)
    close(pb)

    message(.msg("animation_saved", file_path, total_duration, fps))

    invisible(file_path)
}
