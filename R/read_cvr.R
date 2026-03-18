#' Read a CVR file and append its election_id
#'
#' @param file        Path to a CVR CSV file for a single election.
#' @param identifiers Path to cvr_identifiers.csv (default: package-level copy).
#'
#' @return A tibble of the CVR data with an `election_id` column prepended.
#'   If no match is found in the identifiers table the column will be `NA`.
#' @export
read_cvr <- function(file,
                     identifiers = system.file("extdata", "cvr_identifiers.csv",
                                               package = "rcvr")) {

  # Fall back to project-root copy when not installed as a package
  if (!nzchar(identifiers) || !file.exists(identifiers)) {
    identifiers <- "cvr_identifiers.csv"
  }

  # ---- 1. Load identifiers table -----------------------------------------
  id_tbl <- readr::read_csv(identifiers, show_col_types = FALSE)

  # ---- 2. Match on bare file name (no directory, no extension) -----------
  base_name <- sub("\\.(csv|tab)$", "", basename(file), ignore.case = TRUE)

  matched <- id_tbl |>
    dplyr::filter(source_file == base_name) |>
    dplyr::pull(election_id)

  election_id_val <- if (length(matched) == 1L) matched else NA_character_

  # ---- 3. Read CVR data ---------------------------------------------------
  cvr <- readr::read_csv(file, show_col_types = FALSE)

  # ---- 4. Prepend election_id ---------------------------------------------
  cvr <- cvr |>
    dplyr::mutate(election_id = election_id_val, .before = 1)

  cvr
}
