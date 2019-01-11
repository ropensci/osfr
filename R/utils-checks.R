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
  match.arg(x, tolower(categories))
}
