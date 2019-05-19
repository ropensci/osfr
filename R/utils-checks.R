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
#' @noRd
check_files <- function(files) {
  found <- fs::file_exists(files)
  if (any(!found)) {
    abort(paste0(
      "Can't find find following files/folders included in `path`:\n",
      paste0(
        sprintf("  * %s", names(Filter(isFALSE, found))),
        collapse = "\n")
    ))
  }
  files
}
