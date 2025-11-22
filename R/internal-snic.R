#' Internal SNIC helpers
#'
#' Developer-facing utilities wrapping the SNIC container metadata and the
#' native segmentation entry point.
#'
#' @section Functions:
#' \itemize{
#'   \item \code{.snic_new(seg)} Packs the segmentation array and
#'     metadata into an S3 container.
#'   \item \code{.snic_check(x)} Validates \code{snic} objects before
#'     accessing their slots.
#'   \item \code{.snic_seg(x)}, \code{.snic_means(x)},
#'     \code{.snic_centroids(x)} Accessors returning the segmentation map,
#'     feature means, and centroids respectively.
#'   \item \code{.snic_animation(...)} Internal driver used by
#'     \code{snic_animation()} to orchestrate frame generation.
#'   \item \code{.snic_core(arr, seeds_rc, compactness)} Thin wrapper
#'     around the native \code{snic_snic} routine that performs the actual
#'     clustering.
#' }
#'
#' @keywords internal
#' @name internal_snic
NULL

#' @rdname internal_snic
.snic_new <- function(seg) {
    seg <- .check_x(seg)
    seg_attrs <- attributes(seg)
    attributes(seg) <- seg_attrs[!grepl("^snic\\.", names(seg_attrs))]
    seg_attrs <- seg_attrs[grepl("^snic\\.", names(seg_attrs))]
    names(seg_attrs) <- gsub("^snic\\.", "", names(seg_attrs))
    structure(c(list(seg = seg), seg_attrs), class = "snic")
}

#' @rdname internal_snic
.snic_check <- function(x) {
    if (!inherits(x, "snic")) {
        stop(.msg("snic_expected_class"), call. = FALSE)
    }
    if (!is.list(x)) {
        stop(.msg("snic_expected_list"), call. = FALSE)
    }
    if (!"seg" %in% names(x)) {
        stop(.msg("snic_expected_seg"), call. = FALSE)
    }
    x
}

#' @rdname internal_snic
.snic_seg <- function(x) {
    .check_x(x$seg)
}

#' @rdname internal_snic
.snic_means <- function(x) {
    x$means
}

#' @rdname internal_snic
.snic_centroids <- function(x) {
    x$centroids
}

#' @rdname internal_snic
.snic_animation <- function(x,
                            seeds,
                            file_path,
                            n_cycles,
                            delay,
                            progress,
                            plot_args,
                            snic_args,
                            device_args) {
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

    pb <- if (progress) {
        utils::txtProgressBar(max = n_cycles + 10L, style = 3)
    } else {
        NULL
    }
    frame_files <- character(n_cycles)
    user_device_args <- device_args
    for (i in seq_len(n_cycles)) {
        if (!is.null(pb)) {
            utils::setTxtProgressBar(pb, i)
        }
        current_seeds <- seeds[seq_len(i), , drop = FALSE]
        snic_args <- .modify_list(
            snic_args,
            list(x = x, seeds = current_seeds)
        )
        seg <- do.call(snic, snic_args)

        frame_file <- file.path(frame_dir, sprintf("frame-%02d.png", i))
        frame_files[[i]] <- frame_file

        default_device_args <- list(height = h, width = w, units = "px")
        device_args <- .modify_list(default_device_args, user_device_args)

        device_args$filename <- frame_file
        suppressWarnings(do.call(grDevices::png, device_args))
        tryCatch(
            {
                plot_args <- .modify_list(
                    plot_args,
                    list(x = x, seeds = current_seeds, seg = seg)
                )
                do.call(snic_plot, plot_args)
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
    if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, n_cycles + 10L)
        close(pb)
    }

    message(.msg("animation_saved", file_path, total_duration, fps))

    invisible(file_path)
}

#' @rdname internal_snic
.snic_core <- function(arr, seeds_rc, compactness) {
    if ((!is.array(arr) || !is.numeric(arr) || length(dim(arr)) != 3)) {
        stop(.msg("img_must_be_numeric_array_three_dimensions"))
    }
    if (is.integer(arr)) {
        storage.mode(arr) <- "double"
    }

    if (.seeds_type(seeds_rc) != "rc") {
        stop(.msg("seeds_invalid_type"))
    }

    if (!nrow(seeds_rc)) {
        stop(.msg("seeds_must_have_coordinates"))
    }
    seeds_rc <- as.matrix(round(seeds_rc))
    storage.mode(seeds_rc) <- "integer"

    compactness <- as.numeric(compactness)

    .call("snic_snic", arr, seeds_rc, compactness, "F")
}
