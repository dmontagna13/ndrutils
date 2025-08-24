#' Compartment co-association with density-normalized proximity
#'
#' Build per-cell "positive compartments" for two object types, test cross-type
#' proximity against density-derived random expectation, summarize by well and
#' genotype, and return a plot plus exemplar cells closest to genotype means.
#'
#' @param df A data frame with columns:
#'   `genotype`, `well`, `field`, `unique.cell`, `object.id`,
#'   `object.type` (exactly two unique values), `x.coord`, `y.coord`,
#'   `corr.intensity`.
#' @param px Numeric. Microns per pixel (default 0.149).
#' @param ci.cutoff Numeric scalar or vector. Global corrected-intensity cutoff(s).
#' @param alpha_pair Numeric in (0,1). Significance to merge same-type puncta into compartments.
#' @param alpha_cross Numeric in (0,1). Significance for cross-type proximity (src -> tgt).
#' @param show_progress Logical. Show CLI progress bars (default TRUE).
#' @param title_rel_height Numeric. Relative height of title row in cowplot grid.
#' @param return_exemplars Logical. If TRUE, returns exemplar cell coordinate lists.
#'
#' @return If `length(ci.cutoff) == 1`, a list with:
#'   - `obj1_name`, `obj2_name` (character)
#'   - `well_summary` (long, per-well table)
#'   - `final.plot` (cowplot/ggplot object)
#'   - `exemplars` (nested list: readout -> genotype -> list of up to 5 data frames)
#'
#'   If `length(ci.cutoff) > 1`, a named list of such lists (names like `"ci=2"`),
#'   with attributes `"obj1_name"` and `"obj2_name"`.
#'
#' @examples
#' \dontrun{
#'   res <- ndrutils::compartment_coassoc(df, px = 0.149, ci.cutoff = c(0, 2))
#'   res$`ci=2`$final.plot
#' }
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats na.omit sd
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom purrr map pmap
#' @importFrom tibble tibble

compartment_coassoc <- function(
  df,
  px = 0.149,
  ci.cutoff = 0,
  alpha_pair  = 0.05,
  alpha_cross = 0.05,
  show_progress   = TRUE,
  title_rel_height = 0.08,
  return_exemplars = TRUE
) {

  # ---------- sanity checks ----------
  required_cols <- c("genotype","well","field","unique.cell","object.id",
                     "object.type","x.coord","y.coord","corr.intensity")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop("df is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # enforce exactly two object types
  types_all <- unique(df$object.type)
  if (length(types_all) != 2L) {
    stop("df$object.type must have exactly TWO unique values (found ",
         length(types_all), ").", call. = FALSE)
  }
  type1 <- as.character(types_all[1])
  type2 <- as.character(types_all[2])

  # drop NA key rows
  na_key_rows <- df %>%
    dplyr::filter(is.na(genotype) | is.na(well) | is.na(field) | is.na(unique.cell))
  if (nrow(na_key_rows) > 0) {
    cli::cli_alert_warning("{nrow(na_key_rows)} rows with NA in key columns were dropped.")
    df <- df %>%
      dplyr::filter(!is.na(genotype) & !is.na(well) & !is.na(field) & !is.na(unique.cell))
  }

  # ---------- helpers ----------
  .hull_area_px2 <- function(x, y) {
    x <- as.numeric(x); y <- as.numeric(y)
    if (length(x) < 3L || anyNA(c(x, y))) {
      dx <- diff(range(x, na.rm = TRUE)); dy <- diff(range(y, na.rm = TRUE))
      a  <- dx * dy
      return(if (!is.finite(a) || a <= 0) 1e-6 else a + 1e-6)
    } else {
      idx <- grDevices::chull(x, y)
      xh <- x[idx]; yh <- y[idx]
      area <- 0.5 * abs(sum(xh * c(yh[-1], yh[1]) - yh * c(xh[-1], xh[1])))
      return(if (!is.finite(area) || area <= 0) 1e-6 else area)
    }
  }

  .hull_centroid_px <- function(x, y) {
    x <- as.numeric(x); y <- as.numeric(y)
    if (length(x) < 3L || anyNA(c(x, y))) {
      cx <- mean(range(x, na.rm = TRUE))
      cy <- mean(range(y, na.rm = TRUE))
      return(c(cx = ifelse(is.finite(cx), cx, 0), cy = ifelse(is.finite(cy), cy, 0)))
    }
    idx <- grDevices::chull(x, y)
    xh <- x[idx]; yh <- y[idx]
    x2 <- c(xh, xh[1]); y2 <- c(yh, yh[1])
    cross <- x2[-length(x2)] * y2[-1] - x2[-1] * y2[-length(y2)]
    A <- sum(cross) / 2
    if (!is.finite(A) || abs(A) < 1e-12) {
      cx <- mean(range(x, na.rm = TRUE)); cy <- mean(range(y, na.rm = TRUE))
      return(c(cx = ifelse(is.finite(cx), cx, 0), cy = ifelse(is.finite(cy), cy, 0)))
    }
    Cx <- sum((x2[-length(x2)] + x2[-1]) * cross) / (6 * A)
    Cy <- sum((y2[-length(y2)] + y2[-1]) * cross) / (6 * A)
    c(cx = Cx, cy = Cy)
  }

  .sig_radius <- function(lambda, alpha) {
    ifelse(lambda > 0, sqrt(-log1p(-alpha) / (pi * lambda)), Inf)
  }

  build_positive_compartments <- function(marker_df, cell_area_tbl, marker_name,
                                          alpha_pair = 0.05, show_progress = TRUE) {
    DT  <- data.table::as.data.table(marker_df)
    CA  <- data.table::as.data.table(cell_area_tbl)

    # sanity: required columns present?
    if (!"unique.cell" %in% names(DT)) stop("marker_df is missing 'unique.cell'")
    if (!all(c("unique.cell","area_px2","genotype","well","field") %in% names(CA))) {
      stop("cell_area_tbl must contain 'unique.cell', 'area_px2', 'genotype', 'well', 'field'")
    }

    # key both tables on unique.cell for fast/safe subsetting
    data.table::setkey(DT, unique.cell)
    data.table::setkey(CA, unique.cell)

    ucs <- CA$unique.cell
    out_list <- vector("list", length(ucs))

    if (isTRUE(show_progress)) {
      pb_id <- cli::cli_progress_bar(paste0("Building ", marker_name, " compartments"),
                                     total = length(ucs), .auto_close = FALSE, clear = FALSE)
      on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
    }

    for (i in seq_along(ucs)) {
      uc <- ucs[i]
      g  <- DT[.(uc)]                     # all puncta from this cell
      ca <- CA[.(uc)]                     # single-row cell area/meta
      area_px2 <- ca$area_px2[[1]]

      if (!nrow(g)) {
        out_list[[i]] <- tibble::tibble(
          genotype    = ca$genotype[[1]],
          well        = ca$well[[1]],
          field       = ca$field[[1]],
          unique.cell = uc,
          comp_id     = character(0),
          comp_x      = numeric(0),
          comp_y      = numeric(0),
          comp_size   = integer(0),
          marker      = marker_name
        )
        if (isTRUE(show_progress)) cli::cli_progress_update(id = pb_id, inc = 1)
        next
      }

      lambda <- nrow(g) / max(area_px2, 1e-6)
      r_eps  <- .sig_radius(lambda, alpha_pair)

      if (nrow(g) == 1L || !is.finite(r_eps) || r_eps <= 0) {
        cent <- tibble::tibble(
          genotype    = g$genotype[1],
          well        = g$well[1],
          field       = g$field[1],
          unique.cell = uc,
          comp_id     = paste0(uc, "__", marker_name, "__", 1L),
          comp_x      = g$x.coord[1],
          comp_y      = g$y.coord[1],
          comp_size   = 1L,
          marker      = marker_name
        )
        out_list[[i]] <- cent
      } else {
        cl <- dbscan::dbscan(as.matrix(g[, c(x.coord, y.coord)]), eps = r_eps, minPts = 1)
        g[, cluster := cl$cluster]
        cent <- tibble::as_tibble(g) %>%
          dplyr::group_by(.data$genotype, .data$well, .data$field, .data$unique.cell, .data$cluster) %>%
          dplyr::summarise(
            comp_x = mean(.data$x.coord),
            comp_y = mean(.data$y.coord),
            comp_size = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            comp_id = paste0(.data$unique.cell, "__", marker_name, "__", .data$cluster),
            marker  = marker_name
          ) %>%
          dplyr::select(.data$genotype, .data$well, .data$field, .data$unique.cell,
                        .data$comp_id, .data$comp_x, .data$comp_y, .data$comp_size, .data$marker)
        out_list[[i]] <- cent
      }

      if (isTRUE(show_progress)) cli::cli_progress_update(id = pb_id, inc = 1)
    }

    dplyr::bind_rows(out_list)
  }

  add_within_nn_comp <- function(comp_tbl, cell_area_tbl) {
    if (!nrow(comp_tbl)) {
      return(comp_tbl %>% dplyr::mutate(nn_comp = NA_real_, rtilde_comp = NA_real_))
    }
    keys <- c("genotype", "well", "field", "unique.cell", "marker")
    comp_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
      dplyr::group_modify(~ {
        h <- .x
        uc <- .y$unique.cell[[1]]
        area_px2 <- cell_area_tbl$area_px2[match(uc, cell_area_tbl$unique.cell)]
        area_px2 <- ifelse(is.finite(area_px2) && area_px2 > 0, area_px2, 1e-6)

        if (nrow(h) < 2L) {
          h %>% dplyr::mutate(nn_comp = NA_real_, rtilde_comp = NA_real_)
        } else {
          nn <- RANN::nn2(
            data  = as.matrix(h[, c("comp_x", "comp_y")]),
            query = as.matrix(h[, c("comp_x", "comp_y")]),
            k = 2
          )
          R <- nn$nn.dists[, 2]
          lambda_comp <- nrow(h) / area_px2
          rtilde <- 2 * R * sqrt(lambda_comp)
          h %>% dplyr::mutate(nn_comp = R, rtilde_comp = rtilde)
        }
      }) %>%
      dplyr::ungroup()
  }

  classify_cross <- function(src_comp, tgt_comp, cell_area_tbl,
                             alpha_cross = 0.05, show_progress = TRUE) {
    if (!nrow(src_comp)) {
      return(src_comp %>%
               dplyr::mutate(nn_to_tgt = NA_real_, is_close = FALSE, r_thr_px = NA_real_))
    }
    ucs <- unique(src_comp$unique.cell)
    out <- vector("list", length(ucs))

    if (isTRUE(show_progress)) {
      pb_id <- cli::cli_progress_bar("Cross-type association", total = length(ucs),
                                     .auto_close = FALSE, clear = FALSE)
      on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
    }

    for (i in seq_along(ucs)) {
      uc <- ucs[i]
      s  <- src_comp %>% dplyr::filter(.data$unique.cell == uc)
      t  <- tgt_comp %>% dplyr::filter(.data$unique.cell == uc)
      area_px2 <- cell_area_tbl$area_px2[match(uc, cell_area_tbl$unique.cell)]

      if (!nrow(s) || !nrow(t)) {
        out[[i]] <- s %>%
          dplyr::mutate(nn_to_tgt = NA_real_, is_close = FALSE, r_thr_px = NA_real_)
        if (isTRUE(show_progress)) cli::cli_progress_update(id = pb_id, inc = 1)
        next
      }

      nn <- RANN::nn2(
        data  = as.matrix(t[, c("comp_x","comp_y")]),
        query = as.matrix(s[, c("comp_x","comp_y")]),
        k = 1
      )
      Rst <- as.numeric(nn$nn.dists[, 1])
      lambda_tgt <- nrow(t) / max(area_px2, 1e-6)
      r_thr <- .sig_radius(lambda_tgt, alpha_cross)   # pixels

      out[[i]] <- s %>%
        dplyr::mutate(
          nn_to_tgt = Rst,
          is_close  = is.finite(r_thr) & (Rst <= r_thr),
          r_thr_px  = r_thr
        )
      if (isTRUE(show_progress)) cli::cli_progress_update(id = pb_id, inc = 1)
    }

    dplyr::bind_rows(out)
  }

  # ---------- runner for one ci.cutoff ----------
  run_one <- function(ci) {
    df_ci <- df %>% dplyr::filter(.data$corr.intensity >= ci)

    obj1_df <- df_ci %>% dplyr::filter(.data$object.type == !!type1)
    obj2_df <- df_ci %>% dplyr::filter(.data$object.type == !!type2)

    cell_geom <- df_ci %>%
      dplyr::group_by(.data$unique.cell) %>%
      dplyr::summarise(
        genotype = dplyr::first(stats::na.omit(.data$genotype)),
        well     = dplyr::first(.data$well),
        field    = dplyr::first(.data$field),
        area_px2 = .hull_area_px2(.data$x.coord, .data$y.coord),
        center   = list(.hull_centroid_px(.data$x.coord, .data$y.coord)),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        center_x_px = vapply(.data$center, `[[`, numeric(1), "cx"),
        center_y_px = vapply(.data$center, `[[`, numeric(1), "cy")
      ) %>%
      dplyr::select(-.data$center)

    comp1 <- build_positive_compartments(obj1_df, cell_geom, marker_name = type1,
                                         alpha_pair = alpha_pair, show_progress = show_progress)
    comp2 <- build_positive_compartments(obj2_df, cell_geom, marker_name = type2,
                                         alpha_pair = alpha_pair, show_progress = show_progress)

    comp1 <- add_within_nn_comp(comp1, cell_geom)
    comp2 <- add_within_nn_comp(comp2, cell_geom)

    comp1_cls <- classify_cross(comp1, comp2, cell_geom,
                                alpha_cross = alpha_cross, show_progress = show_progress)
    comp2_cls <- classify_cross(comp2, comp1, cell_geom,
                                alpha_cross = alpha_cross, show_progress = show_progress)

    # dynamic names/labels
    p1_name <- paste0("p.", type1, ".w.", type2)
    p2_name <- paste0("p.", type2, ".w.", type1)
    n1_name <- paste0("n_", type1, "_comp")
    n2_name <- paste0("n_", type2, "_comp")

    p1_label <- paste(type1, "compartments with", type2, "(signif.)")
    p2_label <- paste(type2, "compartments with", type1, "(signif.)")
    n1_label <- paste0(type1, "+")
    n2_label <- paste0(type2, "+")

    sum1 <- comp1_cls %>%
      dplyr::group_by(.data$genotype, .data$well, .data$field, .data$unique.cell) %>%
      dplyr::summarise(
        !!paste0("n_", type1, "_comp")      := dplyr::n(),
        !!paste0("n_", type1, "_w_", type2) := sum(.data$is_close, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        !!p1_name :=
          dplyr::if_else(.data[[paste0("n_", type1, "_comp")]] > 0,
                         100 * .data[[paste0("n_", type1, "_w_", type2)]] /
                           .data[[paste0("n_", type1, "_comp")]],
                         as.numeric(NA))
      )

    sum2 <- comp2_cls %>%
      dplyr::group_by(.data$genotype, .data$well, .data$field, .data$unique.cell) %>%
      dplyr::summarise(
        !!paste0("n_", type2, "_comp")      := dplyr::n(),
        !!paste0("n_", type2, "_w_", type1) := sum(.data$is_close, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        !!p2_name :=
          dplyr::if_else(.data[[paste0("n_", type2, "_comp")]] > 0,
                         100 * .data[[paste0("n_", type2, "_w_", type1)]] /
                           .data[[paste0("n_", type2, "_comp")]],
                         as.numeric(NA))
      )

    cell_summary <- dplyr::full_join(
      sum1, sum2, by = c("genotype","well","field","unique.cell")
    )

    well_summary <- cell_summary %>%
      dplyr::group_by(.data$genotype, .data$well) %>%
      dplyr::summarise(
        !!p1_name := mean(.data[[p1_name]], na.rm = TRUE),
        !!p2_name := mean(.data[[p2_name]], na.rm = TRUE),
        !!paste0("n.", type1) := mean(.data[[n1_name]], na.rm = TRUE),
        !!paste0("n.", type2) := mean(.data[[n2_name]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(c(p1_name, p2_name, paste0("n.", type1), paste0("n.", type2))),
        names_to = "readout", values_to = "percent"
      ) %>%
      dplyr::mutate(
        readout.type = dplyr::case_when(
          grepl("^p\\.", .data$readout) ~ "p",
          grepl("^n\\.", .data$readout) ~ "n",
          TRUE ~ .data$readout
        ),
        readout = dplyr::case_when(
          .data$readout == p1_name ~ p1_label,
          .data$readout == p2_name ~ p2_label,
          .data$readout == paste0("n.", type1) ~ n1_label,
          .data$readout == paste0("n.", type2) ~ n2_label,
          TRUE ~ .data$readout
        )
      )

    # plotting
    bargraphing <- function(type_code) {
      p <- well_summary %>%
        dplyr::filter(.data$readout.type %in% type_code) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$genotype, y = .data$percent, fill = .data$genotype)) +
        ggplot2::facet_wrap(~ readout, nrow = 1, scales = "free_y") +
        ggplot2::stat_summary(fun = mean, geom = "col", width = 0.6, color = "white", alpha = 0.65) +
        ggplot2::stat_summary(fun.data = mean_sd, geom = "errorbar", width = 0.18, linewidth = 0.6) +
        ggplot2::geom_point(
          position = ggplot2::position_jitter(width = 0.10, height = 0, seed = 1), size = 1
        ) +
        theme_ndrutils(base_size = 8) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 5)),
          axis.text.y = ggplot2::element_text(hjust = 1.5)
        )

      if (type_code == "p") {
        p <- p +
          ggplot2::scale_y_continuous(
            labels = function(x) paste0(round(x, 1), "%"),
            expand = ggplot2::expansion(mult = c(0, 0.1))
          ) +
          ggplot2::labs(
            title = "Compartment co-association (significantly closer-than-random)",
            x = NULL, y = "% of compartments", fill = "Genotype", color = "Well"
          )
      } else if (type_code == "n") {
        p <- p +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
          ggplot2::labs(
            title = "Number of compartments (single identity)",
            x = NULL, y = "# of compartments", fill = "Genotype", color = "Well"
          )
      }

      genos <- well_summary %>% dplyr::distinct(.data$genotype) %>% dplyr::pull()
      genos <- as.character(genos[!is.na(genos)])
      if (length(genos) == 2L && requireNamespace("ggsignif", quietly = TRUE)) {
        p <- p +
          ggsignif::geom_signif(
            stat = "signif",
            comparisons = list(genos),
            map_signif_level = TRUE,
            textsize = 4, margin_top = 0.3, vjust = 0.2
          )
      } else {
        if (length(genos) > 2L) {
          cli::cli_alert_info("More than two genotypes detected; omitting significance brackets.")
        } else if (!requireNamespace("ggsignif", quietly = TRUE)) {
          cli::cli_alert_info("Package 'ggsignif' not available; omitting significance brackets.")
        }
      }
      p
    }

    data_plots <- purrr::map(c("n", "p"), bargraphing)

    titleplot <- cowplot::ggdraw() +
      cowplot::draw_label(
        paste0("Corrected Intensity = ", ci, " AU"),
        fontface = "bold", hjust = 0.5
      )

    plotlist <- c(list(titleplot), data_plots)

    final.plot <- cowplot::plot_grid(
      plotlist = plotlist,
      ncol = 1,
      rel_heights = c(title_rel_height, rep(1, length(plotlist) - 1))
    )

    # --------- EXEMPLARS ----------
    exemplars <- NULL
    if (isTRUE(return_exemplars)) {
      cell_long <- cell_summary %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(c(p1_name, p2_name, n1_name, n2_name)),
          names_to = "readout_name", values_to = "value"
        ) %>%
        dplyr::mutate(
          readout_label = dplyr::case_when(
            .data$readout_name == p1_name ~ p1_label,
            .data$readout_name == p2_name ~ p2_label,
            .data$readout_name == n1_name ~ n1_label,
            .data$readout_name == n2_name ~ n2_label,
            TRUE ~ .data$readout_name
          )
        )

      geno_means <- cell_long %>%
        dplyr::group_by(.data$genotype, .data$readout_name, .data$readout_label) %>%
        dplyr::summarise(mean_value = mean(.data$value, na.rm = TRUE), .groups = "drop")

      closest_cells <- cell_long %>%
        dplyr::left_join(geno_means, by = c("genotype","readout_name","readout_label")) %>%
        dplyr::filter(is.finite(.data$value), is.finite(.data$mean_value)) %>%
        dplyr::mutate(dev = abs(.data$value - .data$mean_value)) %>%
        dplyr::arrange(.data$genotype, .data$readout_label, .data$dev, .data$unique.cell) %>%
        dplyr::group_by(.data$genotype, .data$readout_label) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::ungroup()

      centers_idx <- cell_geom %>%
        dplyr::select(.data$unique.cell, .data$center_x_px, .data$center_y_px)

      build_cell_exemplar <- function(uc) {
        cx <- centers_idx$center_x_px[match(uc, centers_idx$unique.cell)]
        cy <- centers_idx$center_y_px[match(uc, centers_idx$unique.cell)]
        if (!is.finite(cx)) cx <- 0
        if (!is.finite(cy)) cy <- 0

        obj1 <- obj1_df %>%
          dplyr::filter(.data$unique.cell == uc) %>%
          dplyr::transmute(
            genotype, well, field, unique.cell,
            kind = "obj1",
            id   = .data$object.id,
            x_rel_um = (.data$x.coord - cx) * px,
            y_rel_um = (.data$y.coord - cy) * px,
            comp_size = as.integer(NA),
            is_close  = NA,
            r_thr_px  = NA_real_
          )

        obj2 <- obj2_df %>%
          dplyr::filter(.data$unique.cell == uc) %>%
          dplyr::transmute(
            genotype, well, field, unique.cell,
            kind = "obj2",
            id   = .data$object.id,
            x_rel_um = (.data$x.coord - cx) * px,
            y_rel_um = (.data$y.coord - cy) * px,
            comp_size = as.integer(NA),
            is_close  = NA,
            r_thr_px  = NA_real_
          )

        c1 <- comp1_cls %>%
          dplyr::filter(.data$unique.cell == uc) %>%
          dplyr::transmute(
            genotype, well, field, unique.cell,
            kind = "comp1",
            id   = .data$comp_id,
            x_rel_um = (.data$comp_x - cx) * px,
            y_rel_um = (.data$comp_y - cy) * px,
            comp_size = as.integer(.data$comp_size),
            is_close  = .data$is_close,
            r_thr_px  = .data$r_thr_px
          )

        c2 <- comp2_cls %>%
          dplyr::filter(.data$unique.cell == uc) %>%
          dplyr::transmute(
            genotype, well, field, unique.cell,
            kind = "comp2",
            id   = .data$comp_id,
            x_rel_um = (.data$comp_x - cx) * px,
            y_rel_um = (.data$comp_y - cy) * px,
            comp_size = as.integer(.data$comp_size),
            is_close  = .data$is_close,
            r_thr_px  = .data$r_thr_px
          )

        dplyr::bind_rows(obj1, obj2, c1, c2)
      }

      exemplars <- closest_cells %>%
        dplyr::group_by(.data$readout_label, .data$genotype) %>%
        dplyr::summarise(
          cells = list(unique(.data$unique.cell)),
          .groups = "drop"
        ) %>%
        purrr::pmap(function(readout_label, genotype, cells) {
          dfs <- purrr::map(cells, build_cell_exemplar)
          list(readout = readout_label, genotype = genotype, dfs = dfs)
        }) %>%
        {
          out <- list()
          for (elt in .) {
            if (is.null(out[[elt$readout]])) out[[elt$readout]] <- list()
            names(elt$dfs) <- as.character(seq_along(elt$dfs))
            out[[elt$readout]][[as.character(elt$genotype)]] <- elt$dfs
          }
          out
        }
    }

    list(
      obj1_name    = type1,
      obj2_name    = type2,
      well_summary = well_summary,
      final.plot   = final.plot,
      exemplars    = exemplars
    )
  }

  # ---------- single or multiple cutoffs ----------
  if (length(ci.cutoff) == 1L) {
    return(run_one(ci.cutoff))
  } else {
    if (isTRUE(show_progress)) {
      pb_id <- cli::cli_progress_bar("Running across intensity cutoffs",
                                     total = length(ci.cutoff),
                                     .auto_close = FALSE, clear = FALSE)
      on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
    }
    res <- purrr::map(ci.cutoff, function(ci) {
      out <- run_one(ci)
      if (isTRUE(show_progress)) cli::cli_progress_update(id = pb_id, inc = 1)
      out
    })
    names(res) <- paste0("ci=", ci.cutoff)
    attr(res, "obj1_name") <- type1
    attr(res, "obj2_name") <- type2
    return(res)
  }
}
