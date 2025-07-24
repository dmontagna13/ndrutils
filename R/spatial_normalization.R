#' Spatially normalise plate data using a GAM surface trained on a uniformity plate
#'
#' @description
#' Fits a thin‑plate spline (via \code{mgcv::gam}) to the well values of a
#' designated \code{uniformity_plate} and uses the predicted surface to normalise
#' one or more target plates. The function is pipe‑friendly:
#'
#' \preformatted{
#'   df %>%
#'     spatial_normalization(uniformity_plate = "U1", apply_to = c("P3","P4"))
#' }
#'
#' Only the plates listed in \code{apply_to} are modified. For those plates,
#' \code{val} is divided by the corresponding predicted surface value at that
#' well position. All other rows are returned unchanged. The function infers
#' row/column indices from the \code{well} string (e.g. "A01", "P24").
#'
#' @param data A data frame containing at least the columns \code{well},
#'   \code{val}, and \code{plate}. The plate layout is assumed to be consistent
#'   (e.g. all 96‑well or all 384‑well).
#' @param uniformity_plate A single value (character/ numeric) identifying the
#'   plate used to train the spatial surface.
#' @param apply_to A vector of plate identifiers to normalise using the trained
#'   surface. Defaults to all plates except \code{uniformity_plate}.
#' @param k_prop Proportion of data points used to set the GAM basis dimension
#'   (\code{k}); default \code{0.6}. \code{k = floor(n_points * k_prop)}.
#' @param plot Logical; if \code{TRUE}, returns a list with the modified data
#'   and two ggplot objects (surface tile plots). Default \code{FALSE}.
#'
#' @return If \code{plot = FALSE}, returns a data frame the same size and shape
#'   as \code{data}, with normalised \code{val} for the specified plates.
#'   If \code{plot = TRUE}, returns a list:
#'   \itemize{
#'     \item \code{data}: the modified data frame
#'     \item \code{surface_plot}: ggplot of the fitted uniformity surface
#'     \item \code{example_plate_plot}: ggplot of one example normalised plate
#'   }
#'
#' @export
#'
#' @importFrom dplyr mutate select rename filter left_join case_when across
#' @importFrom dplyr %>% if_else
#' @importFrom tidyr pivot_longer
#' @importFrom mgcv gam s
#' @importFrom ggplot2 ggplot geom_tile aes labs theme_classic
#' @importFrom magrittr %>%
spatial_normalization <- function(data,
                                  uniformity_plate,
                                  apply_to      = NULL,
                                  k_prop        = 0.6,
                                  plot          = FALSE) {

  ## --------------------- sanity checks --------------------------------------
  if (!all(c("well", "val", "plate") %in% names(data))) {
    stop("`data` must contain columns: well, val, plate.", call. = FALSE)
  }
  if (length(uniformity_plate) != 1L) {
    stop("`uniformity_plate` must be a single value.", call. = FALSE)
  }
  if (!uniformity_plate %in% data$plate) {
    stop("`uniformity_plate` not found in `data$plate`.", call. = FALSE)
  }

  if (is.null(apply_to)) {
    apply_to <- setdiff(unique(data$plate), uniformity_plate)
  }

  ## --------------------- helper: parse well code ----------------------------
  well_to_xy <- function(well) {
    # Extract row letters and column numbers
    row_chr <- gsub("[0-9]+", "", well)
    col_chr <- gsub("[^0-9]", "", well)
    # Map row letters A->1, B->2, etc.
    y <- match(row_chr, LETTERS)
    x <- as.integer(col_chr)
    data.frame(well = well, x = x, y = y, stringsAsFactors = FALSE)
  }

  ## --------------------- build training surface -----------------------------
  uni_df <- data %>%
    dplyr::filter(plate == uniformity_plate) %>%
    dplyr::select(well, val) %>%
    dplyr::mutate(val = val/mean(uni_df$well))


  # infer x/y for uniformity wells
  xy_uni <- well_to_xy(uni_df$well)
  train_df <- dplyr::left_join(uni_df, xy_uni, by = "well") %>%
    dplyr::rename(z = val)

  n_points <- sum(!is.na(train_df$z))
  k_val    <- max(10L, floor(n_points * k_prop))  # ensure a minimum

  fit <- mgcv::gam(z ~ mgcv::s(x, y, bs = "tp", k = k_val),
                   data = train_df,
                   method = "REML")

  # Predict surface for all wells that appear anywhere in the data
  all_xy <- well_to_xy(unique(data$well))
  surface_pred <- stats::predict(fit,
                                 newdata = all_xy,
                                 type = "response")

  surface_df <- dplyr::mutate(all_xy, surface = surface_pred)

  ## --------------------- apply normalisation --------------------------------
  # Join surface to all data
  out <- data %>%
    dplyr::left_join(surface_df, by = "well") %>%
    dplyr::mutate(
      val = dplyr::if_else(plate %in% apply_to,
                           val / surface,
                           val)
    ) %>%
    dplyr::select(-surface)

  ## --------------------- optional plotting ----------------------------------
  if (plot) {
    # surface tile
    surface_plot <- ggplot2::ggplot(surface_df,
                                    ggplot2::aes(x = x, y = y, fill = surface)) +
      ggplot2::geom_tile() +
      ggplot2::theme_classic() +
      ggplot2::labs(title = "Fitted uniformity surface",
                    x = "Column (x)",
                    y = "Row (y)",
                    fill = "Predicted")

    # pick a first apply_to plate (if any) to visualise original vs normalised
    example_plate <- intersect(apply_to, unique(data$plate))[1]
    example_plot <- NULL
    if (!is.na(example_plate)) {
      ex_before <- data %>%
        dplyr::filter(plate == example_plate) %>%
        dplyr::left_join(well_to_xy(well), by = "well")

      ex_after <- out %>%
        dplyr::filter(plate == example_plate) %>%
        dplyr::left_join(well_to_xy(well), by = "well")

      example_plot <- ggplot2::ggplot(ex_after,
                                      ggplot2::aes(x = x, y = y, fill = val)) +
        ggplot2::geom_tile() +
        ggplot2::theme_classic() +
        ggplot2::labs(title = paste0("Normalised plate: ", example_plate),
                      x = "Column (x)", y = "Row (y)", fill = "val (norm)")
    }

    return(list(data = out,
                surface_plot = surface_plot,
                example_plate_plot = example_plot))
  }

  out
}
