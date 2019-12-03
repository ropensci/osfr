#' Convert waterbutler entity to an OSF entity
#'
#' The metadata returned by waterbutler is a subset of what osf provides so this
#' retrieves the same entity from OSF, return a consistent osf_tbl_file.
#'
#' @param x A list containing the processed JSON output returned by the
#'   waterbutler API that represents a single file or directory
#' @return An `osf_tbl_file`
#' @noRd

wb2osf <- function(x) {
  wb_path <- purrr::chuck(x, "data", "id")
  id <- strsplit(wb_path, split = "/", fixed = TRUE)[[1]][2]
  osf_retrieve_file(id)
}
