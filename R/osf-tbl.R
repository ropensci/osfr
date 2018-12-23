#' OSF Tibbles
#'
#' `osf_tbl` objects are specialized data frames based on the
#' [tibble][tibble::tibble-package] class. Each row of an `osf_tbl` represents a
#' single OSF entity, which could be a user, project, component, directory, or
#' file. An `osf_tbl` must also include the following 3 variables:
#' * `name`: the name or title of the OSF entity
#' * `id`: the unique identifier assigned to the entity by OSF
#' * `meta`: a list-column that stores the processed response returned by the OSF API
#'
#' @section: Acknowledgments:
#' Our implementation of the `osf_tbl` class is based on
#' [dribble][googledrive::dribble] objects from the
#' [googledrive][googledrive::googledrive-package] package.
#'
#' @name osf_tbl
#' @aliases osf_tbl_node osf_tbl_file osf_tbl_user
NULL

osf_tbl <- function(x = NULL, subclass = NULL) {
  if (is.list(x) && rlang::is_empty(x)) x <- NULL

  x <- x %||% tibble::tibble(
    name = character(),
    id = character(),
    metadata = list()
  )
  new_osf_tbl(x, subclass)
}

new_osf_tbl <- function(x, subclass = NULL) {
  stopifnot(inherits(x, "data.frame"))
  tibble::new_tibble(x, subclass = c(subclass, "osf_tbl"))
}

# expects a list, where each item is OSF entity represented as a list
as_osf_tbl <- function(x, subclass = NULL) UseMethod("as_osf_tbl")

as_osf_tbl.default <- function(x, subclass = NULL)
  abort("No methods available to coerce this object into an osf_tbl")

as_osf_tbl.data.frame <- function(x, subclass = NULL) new_osf_tbl(x, subclass)

as_osf_tbl.list <- function(x, subclass = NULL) {

  # handle empty lists returned by e.g. .osf_node_children() for childless nodes
  if (rlang::is_empty(x)) return(osf_tbl(subclass = subclass))

  name_field <- switch(subclass,
    osf_tbl_node = "title",
    osf_tbl_file = "name",
    osf_tbl_user = "full_name"
  )

  # I know what you're thinking: why not just map_df this? This approach is
  # about 30% slower than map_df but doesn't require dplyr. Other suggestions
  # are welcome.
  vars <- purrr::map(x, ~ list(
    name = .x$attributes[[name_field]],
    id = .x$id,
    meta = .x[c("attributes", "links", "relationships")]
  ))

  out <- tibble::new_tibble(purrr::transpose(vars), nrow = length(vars))
  out$name <- as.character(out$name)
  out$id   <- as.character(out$id)
  new_osf_tbl(out, subclass)
}

#' @export
`[.osf_tbl` <- function(x, i, j, drop = FALSE) {
  structure(NextMethod(), class = class(x))
}
