#' Create Dual-Channel Colocalization Raster Image
#'
#' @description
#' Generate a dual-channel visualization (yellow = obj1, magenta = obj2) with
#' ring-marked compartments (cyan for dual-identity). Works directly from an
#' exemplar data frame or by extracting an exemplar from a results list created
#' by \code{compartment_coassoc()}.
#'
#' @param ex_df Data frame with exemplar rows and columns:
#'   \itemize{
#'     \item{\code{x_rel_um}, \code{y_rel_um}}: relative positions in micrometers
#'     \item{\code{kind}}: one of \code{"obj1"}, \code{"obj2"}, \code{"comp1"}, \code{"comp2"}
#'     \item{Optional dual flags}: \code{is_dual_comp}, \code{is_dual}, \code{is_close}, ...
#'     \item{Optional sizes}: \code{comp_size} and/or \code{r_thr_px}
#'     \item{Optional centroid (per-cell, repeated per row)}:
#'           \code{cell_center_x_px}, \code{cell_center_y_px}
#'   }
#' @param px Numeric. Micrometers per pixel (> 0). If using \code{res} mode and
#'   \code{px} is omitted, the function will try to read \code{px} from the
#'   calling environment.
#' @param res Optional \code{compartment_coassoc()} result containing \code{$exemplars}.
#' @param readout,genotype,exemplar.id When using \code{res} mode, these identify
#'   the exemplar to render.
#' @param use_first_if_missing Logical. In \code{res} mode, if any of
#'   \code{readout/genotype/exemplar.id} are missing, choose the first available
#'   entry automatically (useful for quick interactive exploration). Default \code{FALSE}.
#' @param width_px,height_px Output size in pixels. If \code{height_px} is \code{NULL},
#'   it is auto-calculated from the exemplar aspect ratio.
#' @param dpi Rasterization dots-per-inch for the PNG device (default 300).
#' @param bg Background color (character; default "black").
#' @param show_compartments,show_hull Logical toggles (default TRUE).
#' @param margin_um Numeric margin around content in micrometers (default 2).
#' @param invert_y Logical. Reverse the y-axis (default TRUE; image-like).
#' @param obj_point_size,obj_alpha Numeric styling for object points.
#' @param comp_stroke Numeric. Stroke width for compartment rings.
#' @param dual_color Color for dual-identity rings (default "#00FFFF").
#' @param comp_size_range Numeric length-2 vector giving min/max ring sizes (mm)
#'   used to map \code{comp_size} or \code{r_thr_px} to point sizes.
#' @param out_path Optional file path to save a PNG.
#' @param return_plot Logical. If \code{TRUE}, return the ggplot object instead
#'   of a raster image. When \code{FALSE} (default), returns a magick image.
#' @param verbose Logical. Print diagnostic messages.
#' @param ... Currently unused.
#'
#' @return If \code{return_plot = TRUE}, a ggplot object; otherwise a \code{magick-image}.
#'
#' @details
#' \strong{Printing the cell centroid:} when rasterizing (i.e., \code{return_plot = FALSE}),
#' if columns \code{cell_center_x_px} and \code{cell_center_y_px} exist in \code{ex_df},
#' the function prints a single line to the terminal:
#' \code{cell centroid (px): x = <value>, y = <value>}.
#'
#' The centroid values represent the original position of the cell center in the
#' microscopy field, enabling tracking of where exemplar cells were located.
#'
#' @examples
#' \dontrun{
#' # Direct mode
#' img <- coassoc_raster(ex_df = my_exemplar_df, px = 0.149)
#'
#' # Results mode (explicit identifiers)
#' img <- coassoc_raster(
#'   res = res, readout = "p.vps35.w.eea1", genotype = "WT", exemplar.id = 1, px = 0.149
#' )
#'
#' # Results mode (auto-pick first available for quick look)
#' img <- coassoc_raster(res = res, use_first_if_missing = TRUE, px = 0.149)
#'
#' # Return ggplot for further tweaking
#' p <- coassoc_raster(ex_df = my_exemplar_df, px = 0.149, return_plot = TRUE)
#' p + ggplot2::ggtitle("Custom title")
#'
#' # Save high-resolution image with custom styling
#' coassoc_raster(
#'   ex_df = my_exemplar_df,
#'   px = 0.149,
#'   out_path = "exemplar_cell.png",
#'   width_px = 1200,
#'   dpi = 300,
#'   dual_color = "#00CED1"  # dark turquoise
#' )
#' }
#'
#' @seealso
#' \code{\link{compartment_coassoc}} for generating the results object,
#' \code{\link[magick]{image_graph}}, \code{\link[magick]{image_write}}
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon theme_void theme element_rect coord_cartesian scale_y_reverse
#' @importFrom dplyr mutate filter summarise select bind_rows group_by ungroup across all_of
#' @importFrom rlang .data
#' @importFrom grDevices chull dev.off col2rgb
#' @importFrom stats na.omit
coassoc_raster <- function(ex_df = NULL,
                           px = NULL,
                           res = NULL,
                           readout = NULL,
                           genotype = NULL,
                           exemplar.id = NULL,
                           use_first_if_missing = FALSE,
                           width_px = 600,
                           height_px = NULL,
                           dpi = 300,
                           bg = "black",
                           show_compartments = TRUE,
                           show_hull = TRUE,
                           margin_um = 2,
                           invert_y = TRUE,
                           obj_point_size = 0.5,
                           obj_alpha = 0.7,
                           comp_stroke = 0.2,
                           dual_color = "#00FFFF",
                           comp_size_range = c(1.6, 4.0),
                           out_path = NULL,
                           return_plot = FALSE,
                           verbose = FALSE,
                           ...) {

  # --------- Mode detection: extract exemplar if res-mode ---------
  if (!is.null(res) && is.null(ex_df)) {
    if (verbose) message("Using results mode to extract exemplar")
    ex_df <- .extract_exemplar_from_results(
      res = res,
      readout = readout,
      genotype = genotype,
      exemplar.id = exemplar.id,
      use_first_if_missing = isTRUE(use_first_if_missing),
      verbose = verbose
    )
    if (is.null(px)) {
      px <- get0("px", envir = parent.frame(), inherits = TRUE, ifnotfound = NULL)
      if (verbose && !is.null(px)) message(sprintf("px taken from parent env: %.4f", px))
    }
  }

  # --------- Validation ---------
  if (is.null(ex_df)) {
    stop("Provide either `ex_df` directly or `res` + (readout, genotype, exemplar.id).",
         call. = FALSE)
  }
  if (is.null(px) || !is.finite(px) || px <= 0) {
    stop("`px` (µm/px) must be > 0.", call. = FALSE)
  }
  if (!is.numeric(width_px) || width_px <= 0) {
    stop("`width_px` must be positive.", call. = FALSE)
  }
  if (!is.null(height_px) && (!is.numeric(height_px) || height_px <= 0)) {
    stop("`height_px` must be positive or NULL.", call. = FALSE)
  }
  if (!is.numeric(dpi) || dpi <= 0) {
    stop("`dpi` must be positive.", call. = FALSE)
  }
  if (!is.numeric(margin_um) || margin_um < 0) {
    stop("`margin_um` must be >= 0.", call. = FALSE)
  }
  if (!is.numeric(obj_point_size) || obj_point_size <= 0) {
    stop("`obj_point_size` must be > 0.", call. = FALSE)
  }
  if (!is.numeric(obj_alpha) || obj_alpha <= 0 || obj_alpha > 1) {
    stop("`obj_alpha` must be in (0,1].", call. = FALSE)
  }
  if (!is.numeric(comp_stroke) || comp_stroke <= 0) {
    stop("`comp_stroke` must be > 0.", call. = FALSE)
  }
  if (!(is.numeric(comp_size_range) && length(comp_size_range) == 2 && diff(comp_size_range) > 0)) {
    stop("`comp_size_range` must be a numeric length-2 vector with min < max.", call. = FALSE)
  }

  # Validate colors
  tryCatch({
    grDevices::col2rgb(bg)
    grDevices::col2rgb(dual_color)
  },
  error = function(e) {
    stop("Invalid `bg` or `dual_color`. Use a color name or hex string.", call. = FALSE)
  })

  # Required columns in ex_df
  req_cols <- c("x_rel_um", "y_rel_um", "kind")
  miss <- setdiff(req_cols, names(ex_df))
  if (length(miss)) {
    stop("`ex_df` is missing required column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }

  # Must contain some objects
  if (sum(ex_df$kind %in% c("obj1", "obj2")) == 0) {
    stop("`ex_df` must contain at least one 'obj1' or 'obj2' point.", call. = FALSE)
  }

  # --------- Build plot ---------
  p <- .create_dual_channel_plot(
    ex_df             = ex_df,
    px                = px,
    show_compartments = show_compartments,
    show_hull         = show_hull,
    bg                = bg,
    margin_um         = margin_um,
    invert_y          = invert_y,
    obj_point_size    = obj_point_size,
    obj_alpha         = obj_alpha,
    comp_stroke       = comp_stroke,
    dual_color        = dual_color,
    comp_size_range   = comp_size_range,
    verbose           = verbose
  )

  if (isTRUE(return_plot)) {
    if (verbose) message("Returning ggplot object (no rasterization).")
    return(p)
  }

  # --------- Print centroid (once) if columns exist ---------
  .print_cell_centroid(ex_df, verbose)

  # --------- Rasterize ---------
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required for rasterization; install it or use return_plot = TRUE.",
         call. = FALSE)
  }

  img <- .rasterize_plot(
    p         = p,
    ex_df     = ex_df,
    px        = px,
    width_px  = width_px,
    height_px = height_px,
    dpi       = dpi,
    bg        = bg,
    verbose   = verbose
  )

  if (!is.null(out_path)) {
    if (verbose) message(sprintf("Saving image to: %s", out_path))
    magick::image_write(img, path = out_path, format = "png")
  }

  img
}

# ---- Internal helpers ---------------------------------------------------------

#' Extract exemplar from results object
#' @noRd
.extract_exemplar_from_results <- function(res, readout, genotype, exemplar.id,
                                           use_first_if_missing = FALSE, verbose = FALSE) {
  ex_all <- res[["exemplars"]]
  if (is.null(ex_all)) {
    stop("`res` has no $exemplars element.", call. = FALSE)
  }

  # Choose readout
  readouts <- names(ex_all)
  if (is.null(readout) && isTRUE(use_first_if_missing)) {
    readout <- readouts[1]
    if (verbose) message(sprintf("Auto-selected readout: '%s'", readout))
  }
  if (is.null(readout) || !readout %in% readouts) {
    stop(sprintf("Unknown or missing `readout`. Available: %s",
                 paste(readouts, collapse = ", ")), call. = FALSE)
  }

  # Choose genotype
  genos <- names(ex_all[[readout]])
  genotype_chr <- as.character(genotype)
  if (is.null(genotype) && isTRUE(use_first_if_missing)) {
    genotype_chr <- genos[1]
    if (verbose) message(sprintf("Auto-selected genotype: '%s'", genotype_chr))
  }
  if (is.null(genotype_chr) || !genotype_chr %in% genos) {
    stop(sprintf("Unknown or missing `genotype` for '%s'. Available: %s",
                 readout, paste(genos, collapse = ", ")), call. = FALSE)
  }

  # Choose exemplar id
  ex_list <- ex_all[[readout]][[genotype_chr]]
  if (is.null(ex_list) || length(ex_list) == 0) {
    stop(sprintf("No exemplars for '%s' / '%s'.", readout, genotype_chr), call. = FALSE)
  }
  ex_names <- names(ex_list)
  take_name <- NULL

  if (is.null(exemplar.id) && isTRUE(use_first_if_missing)) {
    take_name <- if (!is.null(ex_names) && nzchar(ex_names[1])) ex_names[1] else "1"
    if (verbose) message(sprintf("Auto-selected exemplar.id: '%s'", take_name))
  } else if (is.numeric(exemplar.id)) {
    idx <- as.integer(exemplar.id)
    if (idx < 1 || idx > length(ex_list)) {
      stop(sprintf("`exemplar.id` out of range (1..%d).", length(ex_list)), call. = FALSE)
    }
    take_name <- if (!is.null(ex_names) && nzchar(ex_names[idx])) ex_names[idx] else as.character(idx)
  } else if (is.character(exemplar.id) && exemplar.id %in% names(ex_list)) {
    take_name <- exemplar.id
  } else {
    stop(sprintf("Invalid or missing `exemplar.id`. Use 1..%d or one of: %s",
                 length(ex_list),
                 paste(if (is.null(ex_names)) seq_along(ex_list) else ex_names, collapse = ", ")),
         call. = FALSE)
  }

  ex_df <- ex_list[[take_name]]
  if (is.null(ex_df) || !nrow(ex_df)) {
    stop("Chosen exemplar dataframe is empty.", call. = FALSE)
  }

  if (verbose) {
    message(sprintf("Exemplar '%s' extracted (%s / %s), n = %d rows.",
                    take_name, readout, genotype_chr, nrow(ex_df)))
  }
  ex_df
}

#' Map numeric vector to ring sizes
#' @noRd
.size_from_vec <- function(v, size_range = c(1.6, 4.0)) {
  v_ok <- v[is.finite(v)]
  if (!length(v_ok)) return(rep(mean(size_range), length(v)))
  r <- range(v_ok)
  if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) {
    return(rep(mean(size_range), length(v)))
  }
  s <- (v - r[1]) / (r[2] - r[1])
  size_range[1] + (size_range[2] - size_range[1]) * pmax(0, pmin(1, s))
}

#' Find dual-identity flag column
#' @noRd
.find_dual_flag <- function(df) {
  cand <- intersect(c("is_dual_comp", "is_dual", "is_close", "dual", "is_signif", "is_significant"),
                    names(df))
  if (length(cand)) as.logical(df[[cand[1]]]) else rep(FALSE, nrow(df))
}

#' Get sizes for compartment rings
#' @noRd
.get_sizes <- function(df, size_range = c(1.6, 4.0)) {
  if (!nrow(df)) return(numeric(0))
  if ("comp_size" %in% names(df)) {
    .size_from_vec(df[["comp_size"]], size_range)
  } else if ("r_thr_px" %in% names(df)) {
    .size_from_vec(df[["r_thr_px"]], size_range)
  } else {
    rep(mean(size_range), nrow(df))
  }
}

#' Print cell centroid information
#' @noRd
.print_cell_centroid <- function(ex_df, verbose = FALSE) {
  if (all(c("cell_center_x_px", "cell_center_y_px") %in% names(ex_df))) {
    # Get unique centroid values (should be same for all rows of a cell)
    cx_vals <- unique(stats::na.omit(ex_df$cell_center_x_px))
    cy_vals <- unique(stats::na.omit(ex_df$cell_center_y_px))

    if (length(cx_vals) == 1 && length(cy_vals) == 1) {
      # Format with reasonable precision
      cx_str <- if (is.finite(cx_vals)) sprintf("%.1f", cx_vals) else "NA"
      cy_str <- if (is.finite(cy_vals)) sprintf("%.1f", cy_vals) else "NA"
      message(sprintf("Cell centroid (px): x = %s, y = %s", cx_str, cy_str))
    } else if (length(cx_vals) > 1 || length(cy_vals) > 1) {
      if (verbose) {
        message("Warning: Multiple centroid values found in exemplar data.")
        message(sprintf("  x values: %s", paste(round(cx_vals, 1), collapse = ", ")))
        message(sprintf("  y values: %s", paste(round(cy_vals, 1), collapse = ", ")))
      }
    } else {
      if (verbose) message("Centroid values are NA or missing.")
    }
  } else if (verbose) {
    message("No `cell_center_x_px`/`cell_center_y_px` columns in exemplar data.")
  }
}

#' Create the dual-channel plot
#' @noRd
.create_dual_channel_plot <- function(ex_df, px, show_compartments, show_hull,
                                      bg, margin_um, invert_y, obj_point_size,
                                      obj_alpha, comp_stroke, dual_color,
                                      comp_size_range, verbose = FALSE) {

  # Convert to pixels
  ex_px <- ex_df %>%
    dplyr::mutate(
      x_rel_px = .data$x_rel_um / px,
      y_rel_px = .data$y_rel_um / px
    )

  # Bounds from objects
  bounds <- ex_px %>%
    dplyr::filter(.data$kind %in% c("obj1", "obj2")) %>%
    dplyr::summarise(
      xmin = min(.data$x_rel_px, na.rm = TRUE),
      xmax = max(.data$x_rel_px, na.rm = TRUE),
      ymin = min(.data$y_rel_px, na.rm = TRUE),
      ymax = max(.data$y_rel_px, na.rm = TRUE)
    )

  if (!is.finite(bounds$xmin) || !is.finite(bounds$xmax) ||
      !is.finite(bounds$ymin) || !is.finite(bounds$ymax)) {
    stop("Exemplar has no valid obj1/obj2 puncta to set bounds.", call. = FALSE)
  }

  xlim <- c(bounds$xmin - (margin_um/px), bounds$xmax + (margin_um/px))
  ylim <- c(bounds$ymin - (margin_um/px), bounds$ymax + (margin_um/px))

  # Split by type
  obj1  <- ex_px %>% dplyr::filter(.data$kind == "obj1")
  obj2  <- ex_px %>% dplyr::filter(.data$kind == "obj2")
  comp1 <- ex_px %>% dplyr::filter(.data$kind == "comp1")
  comp2 <- ex_px %>% dplyr::filter(.data$kind == "comp2")

  # Identify dual compartments
  comp1_dual <- if (nrow(comp1)) .find_dual_flag(comp1) else logical(0)
  comp2_dual <- if (nrow(comp2)) .find_dual_flag(comp2) else logical(0)

  comp_dual <- dplyr::bind_rows(
    if (nrow(comp1) && any(comp1_dual)) comp1[comp1_dual, , drop = FALSE] else NULL,
    if (nrow(comp2) && any(comp2_dual)) comp2[comp2_dual, , drop = FALSE] else NULL
  )
  comp1_single <- if (nrow(comp1)) comp1[!comp1_dual, , drop = FALSE] else comp1
  comp2_single <- if (nrow(comp2)) comp2[!comp2_dual, , drop = FALSE] else comp2

  # Calculate convex hull
  hull_df <- NULL
  pts_all <- ex_px %>%
    dplyr::filter(.data$kind %in% c("obj1", "obj2")) %>%
    dplyr::select(.data$x_rel_px, .data$y_rel_px)

  if (isTRUE(show_hull) && nrow(pts_all) >= 3) {
    hull_idx <- grDevices::chull(pts_all$x_rel_px, pts_all$y_rel_px)
    hull_df <- pts_all[hull_idx, , drop = FALSE]
  }

  # Get compartment sizes
  comp1_sizes <- .get_sizes(comp1_single, comp_size_range)
  comp2_sizes <- .get_sizes(comp2_single, comp_size_range)
  dual_sizes  <- .get_sizes(comp_dual, comp_size_range)

  # Define colors
  col_obj1  <- "#FFFF00"  # yellow
  col_obj2  <- "#FF00FF"  # magenta
  col_comp1 <- col_obj1
  col_comp2 <- col_obj2
  col_hull  <- "grey30"

  if (verbose) {
    message(sprintf("  obj1: %d points, obj2: %d points", nrow(obj1), nrow(obj2)))
    message(sprintf("  comp1: %d (%d single, %d dual)",
                    nrow(comp1), nrow(comp1_single), sum(comp1_dual)))
    message(sprintf("  comp2: %d (%d single, %d dual)",
                    nrow(comp2), nrow(comp2_single), sum(comp2_dual)))
  }

  # Build plot
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg, colour = bg))

  # Add hull
  if (!is.null(hull_df) && nrow(hull_df) >= 3) {
    p <- p + ggplot2::geom_polygon(
      data = hull_df,
      ggplot2::aes(x = .data$x_rel_px, y = .data$y_rel_px),
      fill = NA,
      colour = col_hull,
      linewidth = 0.4
    )
  }

  # Add object points
  if (nrow(obj1)) {
    p <- p + ggplot2::geom_point(
      data = obj1,
      ggplot2::aes(x = .data$x_rel_px, y = .data$y_rel_px),
      colour = col_obj1,
      size = obj_point_size,
      alpha = obj_alpha
    )
  }

  if (nrow(obj2)) {
    p <- p + ggplot2::geom_point(
      data = obj2,
      ggplot2::aes(x = .data$x_rel_px, y = .data$y_rel_px),
      colour = col_obj2,
      size = obj_point_size,
      alpha = obj_alpha
    )
  }

  # Add compartment rings (single-identity)
  if (isTRUE(show_compartments)) {
    if (nrow(comp1_single)) {
      p <- p + ggplot2::geom_point(
        data = comp1_single,
        ggplot2::aes(x = .data$x_rel_px, y = .data$y_rel_px),
        shape = 21,
        stroke = comp_stroke,
        size = comp1_sizes,
        colour = col_comp1,
        fill = NA
      )
    }

    if (nrow(comp2_single)) {
      p <- p + ggplot2::geom_point(
        data = comp2_single,
        ggplot2::aes(x = .data$x_rel_px, y = .data$y_rel_px),
        shape = 21,
        stroke = comp_stroke,
        size = comp2_sizes,
        colour = col_comp2,
        fill = NA
      )
    }

    # Add dual-identity compartments with thicker stroke
    if (nrow(comp_dual)) {
      p <- p + ggplot2::geom_point(
        data = comp_dual,
        ggplot2::aes(x = .data$x_rel_px, y = .data$y_rel_px),
        shape = 21,
        stroke = comp_stroke * 1.5,
        size = dual_sizes,
        colour = dual_color,
        fill = NA
      )
    }
  }

  # Set coordinate system (fixed syntax)
  p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)

  if (isTRUE(invert_y)) {
    p <- p + ggplot2::scale_y_reverse()
  }

  return(p)
}

#' Rasterize plot to magick image
#' @noRd
.rasterize_plot <- function(p, ex_df, px, width_px, height_px, dpi, bg, verbose = FALSE) {
  # Auto-calculate height from aspect ratio if needed
  if (is.null(height_px)) {
    ex_px <- ex_df %>%
      dplyr::mutate(
        x_rel_px = .data$x_rel_um / px,
        y_rel_px = .data$y_rel_um / px
      )

    rng <- ex_px %>%
      dplyr::filter(.data$kind %in% c("obj1", "obj2")) %>%
      dplyr::summarise(
        w = diff(range(.data$x_rel_px, na.rm = TRUE)),
        h = diff(range(.data$y_rel_px, na.rm = TRUE))
      )

    asp <- if (is.finite(rng$w) && rng$w > 0) {
      (rng$h + 1e-6) / (rng$w + 1e-6)
    } else {
      1
    }

    height_px <- max(200, round(width_px * asp))

    if (verbose) {
      message(sprintf("Auto-calculated height: %d px (aspect ratio: %.2f)", height_px, asp))
    }
  }

  # Create magick image
  mg <- magick::image_graph(
    width = width_px,
    height = height_px,
    res = dpi,
    bg = bg
  )

  print(p)
  grDevices::dev.off()

  return(mg)
}
