#' A publication‑ready, colour‑blind‑safe theme + palette combo
#'
#' `theme_ndrutils()` returns a **list** containing
#' *a theme* **and** two manual scales (colour & fill).
#' Add it to a plot just like any other theme:
#'
#' ```r
#' ggplot(df, aes(x, y, colour = group)) +
#'   geom_line() +
#'   theme_ndrutils(palette = 3)
#' ```
#'
#' @param base_size Base font size (pts).
#' @param base_line_size Thickness of lines.
#' @param base_rect_size Thickness of rect borders.
#' @param na_colour Colour to use for missing values.
#' @param palette Either an integer 1–5 (built‑in palettes) or a character
#'   vector of colour hex codes.
#'
#' @return A list: `list(theme, scale_colour, scale_fill)`.
#' @export
#'
#' @import ggplot2
theme_ndrutils <- function(base_size      = 11,
                           base_line_size = 0.4,
                           base_rect_size = 0.4,
                           na_colour      = "#BBBBBB",
                           palette        = 1) {

  palettes <- list(
    `1` = c("#000000", "#FF0166", "#117F80", "#40007F", "#66CCFE", "#AA66FF"),
    `2` = c("#FF0166", "#000000", "#117F80", "#40007F", "#66CCFE", "#AA66FF"),
    `3` = c("#117F80", "#FF0166", "#000000", "#40007F", "#66CCFE", "#AA66FF"),
    `4` = c("#000000", "#117F80", "#FF0166", "#40007F", "#66CCFE", "#AA66FF"),
    `5` = c("#40007F", "#66CCFE", "#117F80", "#000000", "#FF0166", "#AA66FF")
  )

  # pick palette
  cols <- if (length(palette) == 1L && is.numeric(palette) || is.character(palette)) {
    id <- as.character(palette)
    if (!id %in% names(palettes))
      stop("`palette` must be 1‑", length(palettes), " or a vector of colours.")
    palettes[[id]]
  } else if (is.vector(palette)) {
    palette
  } else {
    stop("`palette` must be 1‑5 or a colour vector.", call. = FALSE)
  }

  # theme
  base <- ggplot2::theme_classic(
    base_size      = base_size,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace% ggplot2::theme(
    plot.title   = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25), hjust = 0),
    axis.title   = ggplot2::element_text(face = "bold"),
    legend.title = ggplot2::element_text(face = "bold"),
    complete     = TRUE
  )

  list(
    base,
    ggplot2::scale_colour_manual(values = cols, na.value = na_colour),
    ggplot2::scale_fill_manual(values   = cols, na.value = na_colour)
  )
}
