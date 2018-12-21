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
NULL

osf_tbl <- function(x = NULL, subclass = NULL) {
  x <- x %||% list(name = character(), id = character(), metadata = list())
  new_osf_tbl(x, subclass)
}

new_osf_tbl <- function(x, subclass = NULL) {
  tibble::new_tibble(x, subclass = c(subclass, "osf_tbl"))
}

# expects a list, where each item is OSF entity represented as a list
as_osf_tbl <- function(x, subclass = NULL) UseMethod("as_osf_tbl")

as_osf_tbl.default <- function(x, subclass = NULL)
  abort("No methods available to coerce this object into an osf_tbl")

as_osf_tbl.data.frame <- function(x, subclass = NULL) new_osf_tbl(x, subclass)

as_osf_tbl.list <- function(x, subclass = NULL) {

  get_name <- switch(subclass,
    osf_tbl_node = function(x) x$attributes$title,
    osf_tbl_file = function(x) x$attributes$name,
    osf_tbl_user = function(x) x$attributes$full_name
  )

  tbl <- purrr::map_df(
    x,
    ~ list(
      name = get_name(.x),
      id   = .x$id,
      meta = list(.x[c("attributes", "links", "relationships")])
    )
  )

  new_osf_tbl(tbl, subclass)
}

#' @export
`[.osf_tbl` <- function(x, i, j, drop = FALSE) {
  structure(NextMethod(), class = class(x))
}
