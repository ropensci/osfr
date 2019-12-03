#' Find an existing OSF file
#'
#' Currently an internal function for retrieving a specific OSF file or
#' directory by name when the ID is not know.
#'
#' @param x an `osf_tbl` with the node or directory to search within.
#' @param type filter by `"file"` or `"folder"`
#' @param pattern scalar character with the exact name of the file or directory.
#' @return An `osf_tbl_file` with a single row if an exact match for the file
#'    was identified, or zero rows otherwise.
#' @noRd

osf_find_file <- function(x, type, pattern) {
  type <- match.arg(type, c("file", "folder"))
  matches <- osf_ls_files(x, type = type, pattern = pattern)
  matches[matches$name == pattern, ]
}
