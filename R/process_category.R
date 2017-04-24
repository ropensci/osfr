#' Processing whether a category is valid
#'
#' @param category Category to check for validity. Valid categories:
#' \itemize{
#'   \item project
#'   \item hypothesis
#'   \item methods and measures
#'   \item procedure
#'   \item instrumentation
#'   \item data
#'   \item analysis
#'   \item communication
#'   \item other
#' }
#'
#' @return Nothing if succeeded
#' @export
process_category <- function(category = "") {
  if (!category %in% c("project",
                      "hypothesis",
                      "methods and measures",
                      "procedure",
                      "instrumentation",
                      "data",
                      "analysis",
                      "communication",
                      "other")) {
    stop("Please input proper category, see documentation")
  }
}
