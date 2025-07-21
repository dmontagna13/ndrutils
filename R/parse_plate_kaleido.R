#' Parse a Thermo *Kaleido* plate‑reader CSV export
#'
#' Works with the typical Kaleido “grid” output in which the well matrix
#' immediately follows a header line like `",1,2,…,12"`.
#'
#' @param file_path Path to a `.csv` or plain‑text export.
#' @param plate.number Numeric or character identifying the plate.
#'
#' @return A data frame with columns
#'   `file`, `plate`, `well`, `val`.
#' @export
#'
#' @importFrom readr read_csv
parse_plate_kaleido <- function(file_path, plate.number = 1) {

  lines <- readLines(file_path)

  # locate header ",1,2,…" and take next 8–16 rows
  hdr <- grep("^,1(,2){1,11}", lines)[1] + 1L
  if (length(hdr) == 0 || is.na(hdr))
    stop("No grid header found in ", file_path, call. = FALSE)

  grid_raw <- lines[hdr:(hdr + 16L)]

  plate_df <- suppressWarnings(
    readr::read_csv(paste(grid_raw, collapse = "\n"),
                    col_names = FALSE, show_col_types = FALSE)
  )

  nrows <- sum(grepl("^[A-P]$", plate_df[[1]]))
  plate_df <- plate_df[seq_len(nrows), ]

  rows <- plate_df[[1]]
  cols <- sprintf("%02d", seq_len(ncol(plate_df) - 1L))
  well <- as.vector(outer(rows, cols, paste0))

  data.frame(
    file  = file_path,
    plate = as.character(plate.number),
    well  = well,
    val   = as.numeric(as.matrix(plate_df[, -1])),
    stringsAsFactors = FALSE
  )
}
