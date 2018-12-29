#' Assemble filtering options
#'
#' @template filter-pattern
#' @template filter-type
#'
#' @name filters
#' @noRd
NULL

#' @rdname filters
#' @noRd
filter_nodes <- function(pattern = NULL) {
  filters <- list()

  if (is.character(pattern)) {
    filters$title <- pattern
  }

  filter_build(filters)
}

#' @rdname filters
#' @noRd
filter_files <- function(pattern = NULL, type = "any") {
  filters <- list()

  if (type != "any") {
    type <- match.arg(type, c("file", "folder"))
    filters$kind <- type
  }

  if (is.character(pattern)) {
    filters$name <- pattern
  }

  filter_build(filters)
}

#' Convert a list of filters to an API-client query
#'
#' @param filters a list of filters with names that match the filtering options
#'   available for the specific OSF entity
#'
#' @return a list that is empty or comprising API-compliant filtering options
#' @noRd

filter_build <- function(filters) {
  if (!is_empty(filters)) {
    names(filters) <- sprintf("filter[%s]", names(filters))
  }
  filters
}
