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
#'     \item If \code{x} has a CRS: a two-column data frame \code{(lat, lon)}
#'     expressed in \code{EPSG:4326}. These are converted internally to pixel
#'     coordinates before segmentation.
#'   }
#'   Seeds define the starting cluster centers. They must be generated before
#'   calling \code{snic}, typically via \code{\link{snic_grid}} utilities such
#'   as \code{\link{snic_grid_rect}}, \code{\link{snic_grid_hex}}, or
#'   interactively via \code{\link{snic_grid_manual}}.
#'
#' @param compactness Non-negative numeric value controlling the balance
#'   between feature similarity and spatial proximity (default = 0.5).
#'   Larger values produce more spatially compact superpixels.
#'
#' @param ... Reserved for future extensions.
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
#' \code{\link{snic_count_seeds}} for estimating seed counts.
#'
#' @examples
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
#'     s2 <- terra::aggregate(terra::rast(files), fact = 5)
#'
#'     seeds <- snic_grid(
#'         s2,
#'         type    = "rectangular",
#'         spacing = 10L,
#'         padding = 50L
#'     )
#'
#'     seg <- snic(s2, seeds, compactness = 0.25)
#' }
#' @export
snic <- function(x, seeds, compactness = 0.5, ...) {
    x <- check_x(x)
    seeds <- check_seeds(seeds)

    arr <- x_to_arr(x)

    arr <- snic_core(arr, as_seeds_rc(seeds, x), compactness)

    arr_to_x(x, arr, "snic")
}

#' @export
snic_to_array <- function(x) {
    x <- check_x(x)
    x_to_arr(x)
}

#' @export
snic_to_raster <- function(arr, x, names = NULL) {
    arr <- check_x(arr)
    x <- check_x(x)
    arr_to_x(x, arr, names)
}

#' Plot SNIC imagery
#'
#' Render image data processed by SNIC either from in-memory numeric arrays
#' or from \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} objects
#' provided by the \pkg{terra} package. The function supports plotting a
#' single band (default grayscale palette) or a three-channel RGB composite,
#' with optional overlays for seed points and segmentation boundaries.
#'
#' @param x Image data. For the array method this must be a numeric array
#'   with dimensions \code{(height, width, bands)}. For the raster method
#'   the object must be a \code{\link[terra:SpatRaster-class]{SpatRaster}}.
#' @param band Integer index of the band to display when producing a
#'   single-band plot. Defaults to the first band.
#' @param r,g,b Integer indices (1-based) of the bands to use when composing
#'   an RGB plot. All three must be supplied to trigger RGB rendering and the
#'   image must contain at least three bands.
#' @param col Color palette used for single-band plots. Ignored for RGB plots.
#' @param stretch Character string indicating the contrast-stretching method.
#'   Determines how band values are scaled to the \eqn{[0, 1]} range before
#'   plotting. One of:
#'   \itemize{
#'     \item \code{"none"}: no scaling; input values are used directly.
#'     \item \code{"lin"}: linear stretch based on the minimum and maximum
#'       values (default).
#'     \item \code{"hist"}: histogram equalization (redistribute values to
#'       equalize the color histogram).
#'     \item \code{"sd"}: standard deviation stretch (clip to
#'       \eqn{\text{mean} \pm 2 \times \text{sd}}, then scale).
#'   }
#'   Non-numeric arrays or bands with only constant values are plotted as-is.
#' @param ... Additional arguments forwarded to the underlying plotting
#'   function. For arrays, these are passed to
#'   \code{\link[graphics:image]{graphics::image()}}; for raster inputs they
#'   are forwarded to \code{\link[terra:plot]{terra::plot()}} (single band)
#'   or \code{\link[terra:plotRGB]{terra::plotRGB()}} (RGB composites).
#' @param seeds Optional object containing seed coordinates with
#'   columns \code{r} and \code{c}. Additional columns are preserved; when
#'   plotting \code{\link[terra:SpatRaster-class]{SpatRaster}} inputs,
#'   \code{lat} and \code{lon} columns expressed in \code{"EPSG:4326"} (when
#'   present) are projected to the raster's CRS and take precedence over the
#'   provided \code{r}/\code{c} indices.
#' @param seeds_plot_args Optional named list with additional arguments passed
#'   to \code{\link[graphics:points]{graphics::points()}} when drawing
#'   \code{seeds}.
#' @param seg For \code{\link[terra:SpatRaster-class]{SpatRaster}} inputs, an
#'   optional segmentation raster (integer labels) or already vectorized
#'   segments (a \code{\link[terra:SpatVector-class]{terra::SpatVector}}) to
#'   be drawn over the image.
#' @param seg_plot_args Named list of arguments forwarded to
#'   \code{\link[terra:plot]{terra::plot()}} for the \code{seg} overlay. The
#'   argument \code{add = TRUE} is set automatically when not supplied.
#'
#' @return Invisibly, \code{NULL}.
#'
#' @examples
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
#'     # Load and optionally downsample for faster segmentation
#'     s2 <- terra::aggregate(terra::rast(files), fact = 5)
#'
#'     # Generate seeds (lat/lon coordinates because s2 has a CRS)
#'     seeds <- snic_grid(
#'         s2,
#'         type = "rectangular",
#'         spacing = 10L,
#'         padding = 0L
#'     )
#'
#'     # Run segmentation
#'     seg <- snic(s2, seeds = seeds, compactness = 0.1)
#'
#'     # Visualize
#'     snic_plot(
#'         s2,
#'         r = 4, g = 3, b = 1,
#'         stretch = "lin",
#'         seeds = seeds,
#'         seg = seg
#'     )
#' }
#' @export
snic_plot <- function(x,
                      band = 1L,
                      r = NULL,
                      g = NULL,
                      b = NULL,
                      col = grDevices::hcl.colors(
                          128L,
                          palette = "Spectral"
                      ),
                      stretch = "lin",
                      maxcell = 100000L,
                      ...,
                      seeds = NULL,
                      seeds_plot_args = list(
                          pch = 4, col = "#FFFF00", cex = 1
                      ),
                      seg = NULL,
                      seg_plot_args = list(
                          border = "#FFFF00", col = NA, lwd = 0.4
                      )) {
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(.msg("terra_required"), call. = FALSE)
    }

    x <- check_x(x)
    seeds <- check_seeds(seeds)
    if (!is.null(seg)) {
        seg <- check_x(seg, "seg")
    }

    # convert to SpatRaster
    if (!inherits(x, "SpatRaster")) {
        x <- arr_to_x(rast_tmpl(x), x_to_arr(x))
    }

    plot_core(
        x,
        band = band,
        r = r,
        g = g,
        b = b,
        col = col,
        stretch = stretch,
        maxcell = maxcell,
        ...
    )

    if (nrow(seeds)) {
        seeds_xy <- as_seeds_xy(seeds, x)
        plot_grid(seeds_xy, x, seeds_plot_args, add = TRUE)
    }

    if (!is.null(seg)) {
        plot_segments(seg, seg_plot_args, add = TRUE)
    }

    invisible(NULL)
}


#' Animated visualization of SNIC seeding and segmentation
#'
#' Generate an animated GIF illustrating how SNIC segmentation evolves
#' as seeds are progressively added. This function runs a sequence of
#' SNIC segmentation using incremental subsets of the provided seeds
#' and compiles the results into an animation.
#'
#' @param img A `SpatRaster` object (from \pkg{terra}) representing the
#'   image to segment. Dimensions and coordinate reference are inferred
#'   automatically.
#' @param seeds A two-column object specifying the seed coordinates
#'   `(r, c)` in 1-based pixel indices. The object can be a matrix,
#'   data frame, or any object coercible to that structure. Seeds are
#'   typically generated using functions from \code{\link{snic_grid}}
#'   or interactively with \code{\link{snic_grid_manual}}.
#' @param file_path Path where the resulting GIF should be saved. The caller
#'   must supply a writable location; the file must not already exist.
#' @param ... Additional arguments passed to \code{\link{snic_plot}}
#'   when drawing each frame (for example, color palettes or symbol
#'   options for seeds).
#' @param snic_args Named list of additional arguments passed to
#'   \code{\link{snic}} on every iteration. Defaults to
#'   \code{list(compactness = 1)}. Arguments `img` and `seeds`
#'   are reserved and cannot be overridden.
#' @param max_frames Integer giving the maximum number of frames to
#'   render in the animation. If the number of seeds exceeds this
#'   limit, only the first \code{max_frames} seeds are used.
#' @param delay Numeric value specifying the per-frame delay in
#'   centiseconds (1/100 of a second). Passed directly to
#'   \code{magick::image_animate()} to control the playback speed.
#'   Defaults to 10 (0.1 seconds per frame).
#' @param device_args Named list of arguments passed to
#'   \code{grDevices::png()} when rendering frames. Defaults to
#'   \code{list(res = 200, bg = "white")}. Values such as
#'   \code{width}, \code{height}, and \code{filename} are managed
#'   automatically.
#'
#' @details
#' For each iteration, the function adds one seed to the current set
#' and re-runs \code{\link{snic}}. The resulting segmentation and seed
#' locations are drawn using \code{\link{snic_plot}}, saved as a PNG,
#' and finally combined into an animated GIF using the \pkg{magick}
#' package.
#'
#' This tool is intended for exploratory and didactic purposes rather
#' than large-scale processing. It can be used to illustrate the
#' influence of seed placement or the effect of the arguments supplied
#' to `snic()` (for example `compactness`) on segmentation results.
#'
#' @return
#' Invisibly returns the file path of the generated GIF. A message is printed
#' with the saved file name.
#'
#' @seealso
#' \code{\link{snic}}, \code{\link{snic_plot}}, \code{\link{snic_grid}},
#' \code{\link{snic_grid_manual}}.
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
#'     s2 <- terra::aggregate(terra::rast(band_files), factor = 5)
#'
#'     set.seed(42)
#'     seeds <- snic_grid(s2, type = "random", spacing = 10L, padding = 0L)
#'
#'     snic_animation(
#'         s2,
#'         seeds = seeds,
#'         file_path = tempfile("snic-demo", fileext = ".gif"),
#'         max_frames = 150L,
#'         snic_args = list(compactness = 0.1),
#'         r = 4, g = 3, b = 1
#'     )
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
