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

#' Convert OSF entity to waterbutler path ID
#'
#' Discovered around 2022-03, OSF no longer accepts downloads with
#' the GUID. The Waterbutler path ID is now necessary to download
#' files.
#'
#' @param x An `osf_tbl` object
#'
#' @noRd
as_wb_id <- function(x, ...) {
  UseMethod("as_wb_id", x)
}

#' @export
as_wb_id.osf_tbl <- function(x, ...) {
  get_waterbutler_path_id(x)
}