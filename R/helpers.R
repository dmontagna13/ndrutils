#' Mean ± SD summary helper for *ggplot2::stat_summary()*
#'
#' @param x Numeric vector.
#' @return A data frame with columns `y`, `ymin`, `ymax`.
#' @export
mean_sd <- function(x) {
  data.frame(
    y    = mean(x, na.rm = TRUE),
    ymin = y - stats::sd(x, na.rm = TRUE),
    ymax = y + stats::sd(x, na.rm = TRUE)
  )
}

#' Drop perimeter wells from plate‑based data
#'
#' @param .data  Data frame containing `plate`, `row`, and `col` columns.
#' @param apply_to_plates Character vector of plates to trim.
#'   `NULL` = all plates.
#' @param drop Logical; if `FALSE`, returns `.data` untouched.
#' @param col_edges Integer vector of columns to drop (e.g. `c(1, 24)`).
#' @param row_edges Character vector of rows to drop (e.g. `c("A","P")`).
#'
#' @export
#' @importFrom dplyr filter
drop_edges <- function(.data,
                       apply_to_plates = NULL,
                       drop            = TRUE,
                       col_edges       = c(1, 24),
                       row_edges       = c("A", "P")) {

  if (!drop) return(.data)

  plates <- if (is.null(apply_to_plates)) unique(.data$plate) else apply_to_plates

  dplyr::filter(
    .data,
    !(plate %in% plates & (col %in% col_edges | row %in% row_edges))
  )
}
