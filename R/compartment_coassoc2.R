#' Compartment co-association with density-normalized proximity
#' (DT core + null_model + geno_fct + palette/base_size)
#'
#' Fast DT-only internals; configurable null model ("global", "local_kde", "random_labelling");
#' genotype factor ordering via `geno_fct`; and plot styling via `palette` and `base_size`.
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
#' @param null_model Character. One of "global", "local_kde", or "random_labelling".
#' @param bw Character or function. Bandwidth method: "silverman", "scott", "frac", or a custom function.
#' @param bw_frac Numeric. Fraction of cell diameter for bandwidth when bw="frac".
#' @param kde_n Integer. Grid size for KDE (default 128).
#' @param n_kde_min Integer. Minimum points for KDE (default 10).
#' @param kde_shrink_k Numeric. Shrinkage parameter for local KDE (default 20).
#' @param n_sim Integer. Number of permutations for random_labelling (default 199).
#' @param seed Integer. Random seed for reproducibility.
#' @param rl_max_comp_per_cell Integer. Max compartments per cell for random_labelling.
#' @param progress_every Integer. Update progress bar every N cells.
#' @param geno_fct Character vector. Factor levels for genotype ordering.
#' @param palette Integer. Palette ID for theme_ndrutils (default 1).
#' @param base_size Numeric. Base font size for plots (default 8).
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
#' @export
#' @import data.table
#' @importFrom MASS kde2d
#' @importFrom RANN nn2
#' @importFrom dbscan dbscan
#' @importFrom tibble tibble as_tibble
#' @importFrom cowplot ggdraw draw_label plot_grid
#' @importFrom ggplot2 ggplot aes facet_wrap stat_summary geom_point scale_y_continuous labs theme element_text position_jitter expansion margin
#' @importFrom rlang .data
compartment_coassoc2 <- function(
    df,
    px = 0.149,
    ci.cutoff = 0,
    alpha_pair  = 0.05,
    alpha_cross = 0.05,
    show_progress   = TRUE,
    title_rel_height = 0.08,
    return_exemplars = TRUE,
    # --- null model ---
    null_model = c("global","local_kde","random_labelling"),
    # KDE controls
    bw = "silverman",
    bw_frac = 0.12,
    kde_n = 128,
    n_kde_min = 10,
    kde_shrink_k = 20,
    # RL controls
    n_sim = 199,
    seed = NULL,
    rl_max_comp_per_cell = 400,
    # Progress throttling
    progress_every = 50,
    # genotype factor order
    geno_fct = NULL,
    # theme passthrough
    palette = 1L,
    base_size = 8
) {

  null_model <- match.arg(null_model)

  # Handle function-based bandwidth
  bw_func <- NULL
  if (is.function(bw)) {
    bw_func <- bw
    bw <- "custom"
  } else if (is.character(bw)) {
    bw <- match.arg(bw, c("silverman", "scott", "frac"))
  } else {
    stop("bw must be either a character string or a function", call. = FALSE)
  }

  # ---------- sanity checks / parameter guards ----------
  required_cols <- c("genotype","well","field","unique.cell","object.id",
                     "object.type","x.coord","y.coord","corr.intensity")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop("df is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!(is.numeric(alpha_pair) && alpha_pair > 0 && alpha_pair < 1))
    stop("alpha_pair must be in (0,1).", call. = FALSE)
  if (!(is.numeric(alpha_cross) && alpha_cross > 0 && alpha_cross < 1))
    stop("alpha_cross must be in (0,1).", call. = FALSE)
  if (!is.numeric(progress_every) || progress_every < 1)
    stop("progress_every must be >= 1.", call. = FALSE)
  if (!is.numeric(kde_n) || kde_n < 8)
    stop("kde_n must be >= 8.", call. = FALSE)
  if (!is.numeric(bw_frac) || bw_frac <= 0)
    stop("bw_frac must be > 0.", call. = FALSE)
  if (!is.numeric(n_kde_min) || n_kde_min < 1)
    stop("n_kde_min must be >= 1.", call. = FALSE)
  if (!is.numeric(kde_shrink_k) || kde_shrink_k < 0)
    stop("kde_shrink_k must be >= 0.", call. = FALSE)

  # Private DT copy; normalize key types
  DT <- data.table::as.data.table(data.table::copy(df))
  DT[, `:=`(
    genotype    = as.character(genotype),
    well        = as.character(well),
    field       = as.character(field),
    unique.cell = as.character(unique.cell)
  )]

  # Enforce exactly two object types
  types_all <- unique(DT$object.type)
  if (length(types_all) != 2L) {
    stop("df$object.type must have exactly TWO unique values (found ",
         length(types_all), ").", call. = FALSE)
  }
  type1 <- as.character(types_all[1L])
  type2 <- as.character(types_all[2L])

  # Drop NA key rows
  na_key <- DT[is.na(genotype) | is.na(well) | is.na(field) | is.na(unique.cell)]
  if (nrow(na_key) > 0) {
    if (isTRUE(show_progress)) {
      cli::cli_alert_warning("{nrow(na_key)} rows with NA in key columns were dropped.")
    }
    DT <- DT[!(is.na(genotype) | is.na(well) | is.na(field) | is.na(unique.cell))]
  }

  # ---------- helpers ----------
  hull_area_px2 <- function(x, y) {
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

  hull_centroid_px <- function(x, y) {
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

  sig_radius <- function(lambda, alpha) {
    ifelse(lambda > 0, sqrt(-log1p(-alpha) / (pi * lambda)), Inf)
  }

  # --- KDE utilities ---
  kde_bandwidth_xy <- function(x, y, area_px2, bw, bw_frac, bw_func = NULL) {
    if (!is.null(bw_func)) {
      result <- bw_func(x, y, area_px2)
      if (length(result) == 1L) result <- c(result, result)
      if (length(result) != 2L || any(!is.finite(result)) || any(result <= 0))
        stop("Custom bandwidth must return 1 or 2 positive finite values.", call. = FALSE)
      return(c(hx = max(result[1], .Machine$double.eps),
               hy = max(result[2], .Machine$double.eps)))
    }
    sx <- stats::sd(x, na.rm = TRUE); if (!is.finite(sx) || sx <= 0) sx <- diff(range(x, na.rm = TRUE)) / 6
    sy <- stats::sd(y, na.rm = TRUE); if (!is.finite(sy) || sy <= 0) sy <- diff(range(y, na.rm = TRUE)) / 6
    n  <- sum(is.finite(x) & is.finite(y))
    if (bw == "silverman") {
      hx <- 1.06 * sx * n^(-1/5)
      hy <- 1.06 * sy * n^(-1/5)
    } else if (bw == "scott") {
      hx <- sx * n^(-1/6)
      hy <- sy * n^(-1/6)
    } else if (bw == "frac") {
      diam <- 2 * sqrt(max(area_px2, 1e-6) / pi)
      hx <- hy <- bw_frac * diam
    } else {
      stop("Unknown bandwidth method: ", bw, call. = FALSE)
    }
    c(hx = max(hx, .Machine$double.eps), hy = max(hy, .Machine$double.eps))
  }

  bilinear_interp <- function(grid_x, grid_y, z, xq, yq) {
    ix <- pmax(1L, pmin(length(grid_x) - 1L, findInterval(xq, grid_x, all.inside = TRUE)))
    iy <- pmax(1L, pmin(length(grid_y) - 1L, findInterval(yq, grid_y, all.inside = TRUE)))
    x1 <- grid_x[ix]; x2 <- grid_x[ix + 1L]
    y1 <- grid_y[iy]; y2 <- grid_y[iy + 1L]
    tx <- ifelse(x2 > x1, (xq - x1) / (x2 - x1), 0)
    ty <- ifelse(y2 > y1, (yq - y1) / (y2 - y1), 0)
    z11 <- z[cbind(ix,       iy      )]
    z21 <- z[cbind(ix + 1L,  iy      )]
    z12 <- z[cbind(ix,       iy + 1L )]
    z22 <- z[cbind(ix + 1L,  iy + 1L )]
    (1 - tx) * (1 - ty) * z11 + tx * (1 - ty) * z21 + (1 - tx) * ty * z12 + tx * ty * z22
  }

  local_lambda_at <- function(tx, ty, area_px2, xq, yq, kde_n, bw, bw_frac, n_kde_min, shrink_k, bw_func = NULL) {
    ok <- is.finite(tx) & is.finite(ty)
    tx <- tx[ok]; ty <- ty[ok]
    nt <- length(tx)
    lambda_global <- nt / max(area_px2, 1e-6)
    if (nt < n_kde_min || kde_n < 2L) return(rep(lambda_global, length(xq)))
    lims <- c(range(tx, na.rm = TRUE), range(ty, na.rm = TRUE))
    hb <- kde_bandwidth_xy(tx, ty, area_px2, bw, bw_frac, bw_func)
    kd <- MASS::kde2d(tx, ty, n = as.integer(kde_n), h = hb, lims = c(lims[1], lims[2], lims[3], lims[4]))
    dens <- bilinear_interp(kd$x, kd$y, kd$z, xq, yq)
    lambda_loc <- nt * pmax(dens, 0)
    w <- nt / (nt + shrink_k)
    lambda_tilde <- (1 - w) * lambda_global + w * lambda_loc
    pmax(lambda_tilde, .Machine$double.eps)
  }

  # ---------- DT core helpers ----------
  build_compartments_dt <- function(obj_dt, cell_geom, marker_name, alpha_pair = 0.05, show_progress = TRUE, progress_every = 50) {
    if (!nrow(obj_dt)) {
      return(data.table::data.table(
        genotype    = character(0),
        well        = character(0),
        field       = character(0),
        unique.cell = character(0),
        comp_id     = character(0),
        comp_x      = numeric(0),
        comp_y      = numeric(0),
        comp_size   = integer(0),
        marker      = character(0)
      ))
    }
    data.table::setkey(obj_dt, unique.cell)
    ucs <- unique(obj_dt$unique.cell)
    out <- vector("list", length(ucs))

    pb_id <- NULL
    counter <- 0
    if (isTRUE(show_progress)) {
      pb_id <- cli::cli_progress_bar(paste0("Building ", marker_name, " compartments"),
                                     total = length(ucs), .auto_close = FALSE, clear = FALSE)
      on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
    }

    for (i in seq_along(ucs)) {
      uc <- ucs[i]
      g  <- obj_dt[uc]
      # Filter non-finite coordinates defensively
      g  <- g[is.finite(x.coord) & is.finite(y.coord)]
      counter <- counter + 1
      if (isTRUE(show_progress) && (counter %% progress_every == 0L || i == length(ucs))) {
        cli::cli_progress_update(id = pb_id, inc = min(progress_every, counter))
        counter <- 0
      }
      if (!nrow(g)) { out[[i]] <- NULL; next }

      area_px2 <- cell_geom[.(uc), area_px2]
      if (!is.finite(area_px2) || area_px2 <= 0) area_px2 <- 1e-6

      lambda <- nrow(g) / area_px2
      r_eps  <- sig_radius(lambda, alpha_pair)

      if (nrow(g) == 1L || !is.finite(r_eps) || r_eps <= 0) {
        out[[i]] <- g[1L, .(
          genotype, well, field, unique.cell = uc,
          comp_id   = paste0(uc, "__", marker_name, "__1"),
          comp_x    = x.coord, comp_y = y.coord,
          comp_size = 1L, marker = marker_name
        )]
        next
      }

      coords <- cbind(g$x.coord, g$y.coord)
      cl <- dbscan::dbscan(coords, eps = r_eps, minPts = 1L)
      if (length(cl$cluster) != nrow(g)) cl$cluster <- cl$cluster[seq_len(nrow(g))]
      g[, cluster := as.integer(cl$cluster)]

      cent <- g[, .(
        comp_x    = mean(x.coord),
        comp_y    = mean(y.coord),
        comp_size = .N,
        comp_id   = paste0(unique.cell[1L], "__", marker_name, "__", unique(cluster)),
        marker    = marker_name
      ), by = .(genotype, well, field, unique.cell, cluster)][, cluster := NULL][]
      out[[i]] <- cent
    }
    data.table::rbindlist(out, use.names = TRUE, fill = FALSE)
  }

  add_within_nn_comp_dt <- function(comp_dt, cell_geom, null_model, bw, bw_frac, kde_n, n_kde_min, kde_shrink_k, bw_func = NULL) {
    if (!nrow(comp_dt)) {
      return(data.table::copy(comp_dt)[, `:=`(nn_comp = NA_real_, rtilde_comp = NA_real_)])
    }
    data.table::setkey(comp_dt, unique.cell, marker)
    kde_max_comp_per_cell <- 400L  # internal safety: fallback to global if too many per cell

    comp_dt[, c("nn_comp","rtilde_comp") := {
      area <- cell_geom[.(unique.cell[1L]), area_px2]
      if (.N < 2L) {
        list(rep(NA_real_, .N), rep(NA_real_, .N))
      } else {
        ok <- is.finite(comp_x) & is.finite(comp_y)
        cx <- comp_x[ok]; cy <- comp_y[ok]
        if (length(cx) < 2L) {
          list(rep(NA_real_, .N), rep(NA_real_, .N))
        } else {
          nn <- RANN::nn2(data = cbind(cx, cy), query = cbind(cx, cy), k = 2L)
          # Map back into full-length vector
          R <- rep(NA_real_, .N); R[ok] <- nn$nn.dists[, 2]
          use_local <- identical(null_model, "local_kde") && sum(ok) <= kde_max_comp_per_cell
          lam_vec <- if (use_local) {
            lv <- local_lambda_at(cx, cy, area, cx, cy,
                                  kde_n = kde_n, bw = bw, bw_frac = bw_frac,
                                  n_kde_min = n_kde_min, shrink_k = kde_shrink_k,
                                  bw_func = bw_func)
            out <- rep(NA_real_, .N); out[ok] <- lv; out
          } else {
            rep(sum(ok) / max(area, 1e-6), .N)
          }
          list(R, 2 * R * sqrt(lam_vec))
        }
      }
    }, by = .(unique.cell, marker)]
    comp_dt[]
  }

  classify_cross_dt <- function(src, tgt, cell_geom, alpha_cross,
                                null_model, bw, bw_frac, kde_n, n_kde_min, kde_shrink_k,
                                n_sim, seed, rl_max_comp_per_cell,
                                show_progress, progress_every, bw_func = NULL) {
    if (!nrow(src)) {
      return(data.table::copy(src)[, `:=`(nn_to_tgt = NA_real_, is_close = FALSE, r_thr_px = NA_real_, p_empirical = NA_real_)])
    }
    data.table::setkey(src, unique.cell); data.table::setkey(tgt, unique.cell)

    ucs <- unique(src$unique.cell)
    out <- vector("list", length(ucs))

    if (!is.null(seed)) set.seed(seed)

    pb_id <- NULL
    counter <- 0
    if (isTRUE(show_progress)) {
      pb_id <- cli::cli_progress_bar(paste0("Cross-type (", null_model, ")"), total = length(ucs),
                                     .auto_close = FALSE, clear = FALSE)
      on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
    }

    for (i in seq_along(ucs)) {
      uc <- ucs[i]
      s  <- src[uc]; t <- tgt[uc]
      # Filter non-finite comp coordinates
      s  <- s[is.finite(comp_x) & is.finite(comp_y)]
      t  <- t[is.finite(comp_x) & is.finite(comp_y)]
      area <- cell_geom[.(uc), area_px2]

      counter <- counter + 1
      if (isTRUE(show_progress) && (counter %% progress_every == 0L || i == length(ucs))) {
        cli::cli_progress_update(id = pb_id, inc = min(progress_every, counter))
        counter <- 0
      }

      if (!nrow(s) || !nrow(t)) {
        out[[i]] <- s[, `:=`(nn_to_tgt = NA_real_, is_close = FALSE, r_thr_px = NA_real_, p_empirical = NA_real_)]
        next
      }

      nn_obs <- RANN::nn2(data = cbind(t$comp_x, t$comp_y), query = cbind(s$comp_x, s$comp_y), k = 1L)
      Rst_obs <- as.numeric(nn_obs$nn.dists[, 1])

      if (identical(null_model, "global")) {
        lambda_tgt <- nrow(t) / max(area, 1e-6)
        r_thr <- sig_radius(lambda_tgt, alpha_cross)
        res <- s[, `:=`(
          nn_to_tgt = Rst_obs,
          is_close  = is.finite(r_thr) & (Rst_obs <= r_thr),
          r_thr_px  = r_thr,
          p_empirical = NA_real_
        )][]
        out[[i]] <- res

      } else if (identical(null_model, "local_kde")) {
        # internal safety: fallback to global when too many targets
        use_local <- nrow(t) <= 400L
        if (use_local) {
          lam_loc <- local_lambda_at(t$comp_x, t$comp_y, area, s$comp_x, s$comp_y,
                                     kde_n = kde_n, bw = bw, bw_frac = bw_frac,
                                     n_kde_min = n_kde_min, shrink_k = kde_shrink_k,
                                     bw_func = bw_func)
          r_thr_vec <- sig_radius(lam_loc, alpha_cross)
        } else {
          lambda_tgt <- nrow(t) / max(area, 1e-6)
          r_thr_vec  <- rep(sig_radius(lambda_tgt, alpha_cross), nrow(s))
        }
        res <- s[, `:=`(
          nn_to_tgt = Rst_obs,
          is_close  = is.finite(r_thr_vec) & (Rst_obs <= r_thr_vec),
          r_thr_px  = r_thr_vec,
          p_empirical = NA_real_
        )][]
        out[[i]] <- res

      } else { # random_labelling
        ns <- nrow(s); nt <- nrow(t); N <- ns + nt
        if (N > rl_max_comp_per_cell) {
          lambda_tgt <- nt / max(area, 1e-6)
          r_thr <- sig_radius(lambda_tgt, alpha_cross)
          res <- s[, `:=`(
            nn_to_tgt = Rst_obs,
            is_close  = is.finite(r_thr) & (Rst_obs <= r_thr),
            r_thr_px  = r_thr,
            p_empirical = NA_real_
          )][]
          out[[i]] <- res
        } else {
          P <- rbind(
            s[, .(x = comp_x, y = comp_y)],
            t[, .(x = comp_x, y = comp_y)]
          )
          src_idx <- seq_len(ns)
          less_eq <- integer(ns)
          for (b in seq_len(n_sim)) {
            tgt_idx <- sample.int(N, size = nt, replace = FALSE)
            nn_b <- RANN::nn2(data = cbind(P$x[tgt_idx], P$y[tgt_idx]),
                              query = cbind(P$x[src_idx], P$y[src_idx]), k = 1L)
            Rb <- as.numeric(nn_b$nn.dists[, 1])
            less_eq <- less_eq + as.integer(Rb <= Rst_obs)
          }
          p_emp <- (1 + less_eq) / (1 + n_sim)
          res <- s[, `:=`(
            nn_to_tgt = Rst_obs,
            is_close  = (p_emp <= alpha_cross),
            r_thr_px  = NA_real_,
            p_empirical = p_emp
          )][]
          out[[i]] <- res
        }
      }
    }

    data.table::rbindlist(out, use.names = TRUE, fill = FALSE)
  }

  # ---------- runner for one ci.cutoff ----------
  run_one <- function(ci) {
    df_ci <- DT[corr.intensity >= ci]

    obj1_dt <- df_ci[object.type == type1]
    obj2_dt <- df_ci[object.type == type2]

    # Cell geometry
    cell_geom <- df_ci[, {
      A <- hull_area_px2(x.coord, y.coord)
      cxy <- hull_centroid_px(x.coord, y.coord)
      .(genotype = genotype[which.max(!is.na(genotype))[1L]],
        well     = well[1L],
        field    = field[1L],
        area_px2 = if (!is.finite(A) || A <= 0) 1e-6 else A,
        center_x_px = cxy[["cx"]],
        center_y_px = cxy[["cy"]])
    }, by = unique.cell]
    data.table::setkey(cell_geom, unique.cell)

    comp1 <- build_compartments_dt(obj1_dt, cell_geom, marker_name = type1,
                                   alpha_pair = alpha_pair, show_progress = show_progress, progress_every = progress_every)
    comp2 <- build_compartments_dt(obj2_dt, cell_geom, marker_name = type2,
                                   alpha_pair = alpha_pair, show_progress = show_progress, progress_every = progress_every)

    comp1 <- add_within_nn_comp_dt(comp1, cell_geom, null_model, bw, bw_frac, kde_n, n_kde_min, kde_shrink_k, bw_func)
    comp2 <- add_within_nn_comp_dt(comp2, cell_geom, null_model, bw, bw_frac, kde_n, n_kde_min, kde_shrink_k, bw_func)

    comp1_cls <- classify_cross_dt(comp1, comp2, cell_geom, alpha_cross,
                                   null_model, bw, bw_frac, kde_n, n_kde_min, kde_shrink_k,
                                   n_sim, seed, rl_max_comp_per_cell,
                                   show_progress, progress_every, bw_func)
    comp2_cls <- classify_cross_dt(comp2, comp1, cell_geom, alpha_cross,
                                   null_model, bw, bw_frac, kde_n, n_kde_min, kde_shrink_k,
                                   n_sim, seed, rl_max_comp_per_cell,
                                   show_progress, progress_every, bw_func)

    # dynamic names/labels
    p1_name <- paste0("p.", type1, ".w.", type2)
    p2_name <- paste0("p.", type2, ".w.", type1)
    n1_name <- paste0("n_", type1, "_comp")
    n2_name <- paste0("n_", type2, "_comp")

    p1_label <- paste(type1, "compartments with", type2, "(signif.)")
    p2_label <- paste(type2, "compartments with", type1, "(signif.)")
    n1_label <- paste0(type1, "+")
    n2_label <- paste0(type2, "+")

    # Per-cell summaries
    sum1 <- comp1_cls[, .(
      `n_1_comp` = .N,
      `n_1_w_2`  = sum(is_close, na.rm = TRUE)
    ), by = .(genotype, well, field, unique.cell)][
      , `:=`(p1 = fifelse(`n_1_comp` > 0, 100 * `n_1_w_2` / `n_1_comp`, NA_real_))][]
    data.table::setnames(sum1, c("n_1_comp","n_1_w_2","p1"), c(n1_name, paste0("n_", type1, "_w_", type2), p1_name))

    sum2 <- comp2_cls[, .(
      `n_2_comp` = .N,
      `n_2_w_1`  = sum(is_close, na.rm = TRUE)
    ), by = .(genotype, well, field, unique.cell)][
      , `:=`(p2 = fifelse(`n_2_comp` > 0, 100 * `n_2_w_1` / `n_2_comp`, NA_real_))][]
    data.table::setnames(sum2, c("n_2_comp","n_2_w_1","p2"), c(n2_name, paste0("n_", type2, "_w_", type1), p2_name))

    cell_summary <- merge(sum1, sum2, all = TRUE, by = c("genotype","well","field","unique.cell"))

    # Per-well means with dynamic column names
    well_summary_dt <- cell_summary[, {
      v1  <- mean(get(p1_name), na.rm = TRUE)
      v2  <- mean(get(p2_name), na.rm = TRUE)
      vn1 <- mean(get(n1_name), na.rm = TRUE)
      vn2 <- mean(get(n2_name), na.rm = TRUE)
      out <- list(v1, v2, vn1, vn2)
      names(out) <- c(p1_name, p2_name, paste0("n.", type1), paste0("n.", type2))
      out
    }, by = .(genotype, well)]

    long <- data.table::melt(
      well_summary_dt,
      id.vars = c("genotype","well"),
      measure.vars = c(p1_name, p2_name, paste0("n.", type1), paste0("n.", type2)),
      variable.name = "readout",
      value.name = "percent",
      variable.factor = FALSE
    )[
      , readout := as.character(readout)
    ][, `:=`(
      readout.type = fifelse(grepl("^p\\.", readout), "p",
                             fifelse(grepl("^n\\.", readout), "n", readout)),
      readout = fifelse(readout == p1_name, p1_label,
                        fifelse(readout == p2_name, p2_label,
                                fifelse(readout == paste0("n.", type1), n1_label,
                                        fifelse(readout == paste0("n.", type2), n2_label, readout))))
    )]

    # Convert to tibble for downstream plotting
    well_summary <- tibble::as_tibble(long)

    # genotype factor releveling
    if (!is.null(geno_fct) && length(geno_fct)) {
      current_genos <- unique(as.character(well_summary$genotype))
      base_levels <- unique(stats::na.omit(as.character(geno_fct)))
      extras <- sort(setdiff(current_genos, base_levels))
      final_levels <- c(base_levels, extras)
      well_summary$genotype <- factor(well_summary$genotype, levels = final_levels)
    }

    # plotting
    mean_sd <- get("mean_sd", envir = asNamespace("ndrutils"), inherits = FALSE)
    theme_ndrutils <- get("theme_ndrutils", envir = asNamespace("ndrutils"), inherits = FALSE)

    bargraphing <- function(type_code) {
      df_plot <- well_summary[well_summary$readout.type %in% type_code, ]
      p <- ggplot2::ggplot(
        data = df_plot,
        mapping = ggplot2::aes(x = .data$genotype, y = .data$percent, fill = .data$genotype)
      ) +
        ggplot2::facet_wrap(~ readout, nrow = 1, scales = "free_y") +
        ggplot2::stat_summary(fun = mean, geom = "col", width = 0.6, color = "white", alpha = 0.65) +
        ggplot2::stat_summary(fun.data = mean_sd, geom = "errorbar", width = 0.18, linewidth = 0.6) +
        ggplot2::geom_point(
          position = ggplot2::position_jitter(width = 0.10, height = 0, seed = 1), size = 1
        ) +
        theme_ndrutils(palette = palette, base_size = base_size) +
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

      if (nrow(df_plot) > 0 && is.factor(df_plot$genotype) && requireNamespace("ggsignif", quietly = TRUE)) {
        lvl <- levels(droplevels(df_plot$genotype))
        if (length(lvl) == 2L) {
          p <- p +
            ggsignif::geom_signif(
              stat = "signif",
              comparisons = list(lvl),
              map_signif_level = TRUE,
              textsize = 4, margin_top = 0.3, vjust = 0.2
            )
        }
      } else if (!requireNamespace("ggsignif", quietly = TRUE)) {
        cli::cli_alert_info("Package 'ggsignif' not available; omitting significance brackets.")
      }
      p
    }

    data_plots <- list(bargraphing("n"), bargraphing("p"))

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

      # --- NEW: ensure consistent numeric type to avoid melt coercion warning
      # p1_name/p2_name are numeric; n1_name/n2_name may be integer => coerce to numeric
      for (nm in c(p1_name, p2_name, n1_name, n2_name)) {
        if (!is.null(cell_summary[[nm]])) {
          data.table::set(cell_summary, j = nm, value = as.numeric(cell_summary[[nm]]))
        }
      }

      cell_long <- data.table::melt(
        cell_summary,
        id.vars = c("genotype","well","field","unique.cell"),
        measure.vars = c(p1_name, p2_name, n1_name, n2_name),
        variable.name = "readout_name",
        value.name = "value",
        variable.factor = FALSE
      )[
        , readout_name := as.character(readout_name)
      ][, readout_label := fifelse(
        readout_name == p1_name, p1_label,
        fifelse(readout_name == p2_name, p2_label,
                fifelse(readout_name == n1_name, n1_label, n2_label)))
      ]

      geno_means <- cell_long[, .(mean_value = mean(value, na.rm = TRUE)),
                              by = .(genotype, readout_name, readout_label)]

      closest_cells <- merge(cell_long, geno_means,
                             by = c("genotype","readout_name","readout_label"))[
                               is.finite(value) & is.finite(mean_value),
                             ][, dev := abs(value - mean_value)][
                               order(genotype, readout_label, dev, unique.cell)
                             ][, head(.SD, 5), by = .(genotype, readout_label)]

      centers_idx <- df_ci[, {
        cxy <- hull_centroid_px(x.coord, y.coord)
        .(unique.cell = unique.cell[1L], center_x_px = cxy[["cx"]], center_y_px = cxy[["cy"]])
      }, by = unique.cell]
      data.table::setkey(centers_idx, unique.cell)

      build_cell_exemplar <- function(uc, obj1_data, obj2_data, comp1_data, comp2_data) {
        cx <- centers_idx[.(uc), center_x_px]; if (!is.finite(cx)) cx <- 0
        cy <- centers_idx[.(uc), center_y_px]; if (!is.finite(cy)) cy <- 0

        obj1 <- obj1_data[unique.cell == uc & is.finite(x.coord) & is.finite(y.coord), .(
          genotype, well, field, unique.cell,
          kind = "obj1",
          id   = object.id,
          x_rel_um = (x.coord - cx) * px,
          y_rel_um = (y.coord - cy) * px,
          cell_center_x_px = cx,
          cell_center_y_px = cy,
          comp_size = as.integer(NA),
          is_close  = NA,
          r_thr_px  = NA_real_
        )]

        obj2 <- obj2_data[unique.cell == uc & is.finite(x.coord) & is.finite(y.coord), .(
          genotype, well, field, unique.cell,
          kind = "obj2",
          id   = object.id,
          x_rel_um = (x.coord - cx) * px,
          y_rel_um = (y.coord - cy) * px,
          cell_center_x_px = cx,
          cell_center_y_px = cy,
          comp_size = as.integer(NA),
          is_close  = NA,
          r_thr_px  = NA_real_
        )]

        c1 <- comp1_data[unique.cell == uc & is.finite(comp_x) & is.finite(comp_y), .(
          genotype, well, field, unique.cell,
          kind = "comp1",
          id   = comp_id,
          x_rel_um = (comp_x - cx) * px,
          y_rel_um = (comp_y - cy) * px,
          cell_center_x_px = cx,
          cell_center_y_px = cy,
          comp_size = as.integer(comp_size),
          is_close,
          r_thr_px
        )]

        c2 <- comp2_data[unique.cell == uc & is.finite(comp_x) & is.finite(comp_y), .(
          genotype, well, field, unique.cell,
          kind = "comp2",
          id   = comp_id,
          x_rel_um = (comp_x - cx) * px,
          y_rel_um = (comp_y - cy) * px,
          cell_center_x_px = cx,
          cell_center_y_px = cy,
          comp_size = as.integer(comp_size),
          is_close,
          r_thr_px
        )]

        as.data.frame(rbind(obj1, obj2, c1, c2))
      }

      tmp <- closest_cells[, .(cells = list(unique(unique.cell))), by = .(readout_label, genotype)]
      exemplars <- list()
      if (nrow(tmp)) {
        for (i in seq_len(nrow(tmp))) {
          rd <- as.character(tmp$readout_label[i])
          gt <- as.character(tmp$genotype[i])
          cell_ids <- tmp$cells[[i]]
          dfs <- lapply(cell_ids, function(uc) {
            build_cell_exemplar(uc, obj1_dt, obj2_dt, comp1_cls, comp2_cls)
          })
          names(dfs) <- as.character(seq_along(dfs))
          if (is.null(exemplars[[rd]])) exemplars[[rd]] <- list()
          exemplars[[rd]][[gt]] <- dfs
        }
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
    pb_id <- NULL
    if (isTRUE(show_progress)) {
      pb_id <- cli::cli_progress_bar("Running across intensity cutoffs",
                                     total = length(ci.cutoff),
                                     .auto_close = FALSE, clear = FALSE)
      on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
    }
    res <- lapply(ci.cutoff, function(ci) {
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
