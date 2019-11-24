#` Verify node category is valid
#' @param x Scalar character vector specifying the category
#' @return Character vector `x` if the input was valid, errors if not.
#' @references https://developer.osf.io/#operation/nodes_create
#' @noRd
check_category <- function(x) {
  categories <- c(
    "analysis",
    "communication",
    "data",
    "hypothesis",
    "instrumentation",
    "methods and measures",
    "procedure",
    "project",
    "software",
    "other"
  )
  match.arg(tolower(x), categories)
}


#' Verify local file paths exist
#' @param files Character vector containing paths for one or more local files
#' @importFrom fs file_exists path_tidy
#' @noRd
check_files <- function(files) {
  found <- fs::file_exists(files)
  if (any(!found)) {
    abort(paste0(
      "Can't find following files/folders included in `path`:\n",
      paste0(
        sprintf("  * %s", names(Filter(isFALSE, found))),
        collapse = "\n")
    ))
  }
  fs::path_tidy(files)
}

#' Verify a local directory exists
#'
#' @param path A scalar character vector pointing to an existing local directory
#' @importFrom fs dir_exists
#' @return A scalar character vector with the verified `path`. If `path` was
#'   `NULL` a `"."` is returned to represent the cwd.
#' @noRd
check_local_dir <- function(path) {
  if (is.null(path)) {
    path <- "."
  } else {
    if (
      !is_scalar_character(path) ||
      !fs::is_dir(path) ||
      !fs::dir_exists(path)
    ) {
      abort("`path` must point to an existing local directory.")
    }
  }
  fs::path_tidy(path)
}
