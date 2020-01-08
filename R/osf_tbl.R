#' OSF Tibbles
#'
#' Items retrieved from OSF are represented as `osf_tbl` objects, specialized
#' data frames based on the [tibble][tibble::tibble-package] class. See below
#' for additional details.
#'
#' Each row of an `osf_tbl` represents a single OSF entity. This could be a
#' user, project, component, directory, or file. An `osf_tbl` must include
#' the following 3 columns:
#'
#' 1. `name`: indicates the name or title of the entity.
#' 2. `id`: the unique identifier assigned by OSF.
#' 3. `meta`: a list-column that stores the processed response returned by OSF's
#'    API. See the *Meta column* section below for more information.
#'
#' @section Subclasses:
#'
#' `osf_tbl` is the parent class of 3 subclasses that are used to represent
#' each of OSF's main entities:
#'
#' 1. `osf_tbl_user` for users.
#' 2. `osf_tbl_file` for files and directories.
#' 2. `osf_tbl_node` for projects and components.
#'
#' @section OSF nodes:
#'
#' Projects and components are both implemented as *nodes* on OSF. The only
#' distinction between the two is that a project is a top-level node, and a
#' component must have a parent node (i.e., must be a sub-component of another
#' project or component). Because projects and components are functionally
#' identical, osfr uses the same [`osf_tbl_node`] class to represent both.
#'
#' @section Meta column:
#'
#' The `meta` column contains all of the information returned from OSF's API for
#' a single entity, structured as a named list with 3 elements:
#'
#' 1. `attributes` contains metadata about the entity (e.g., names,
#'    descriptions, tags, etc).
#' 2. `links` contains urls to API endpoints with alternative representations of
#'    the entity or actions that may be performed on the entity.
#' 3. `relationships` contains URLs to other entities with relationships to the
#'    entity (e.g., collaborators attached to a project).
#'
#' This information is critical for `osfr`'s internal functions and should not
#' be altered by users. For even more information about these elements, see
#' [OSF's API documentation](https://developer.osf.io/#tag/Entities-and-Entity-Collections).
#'
#' @section Acknowledgments:
#'
#' Our implementation of the `osf_tbl` class is based on `dribble` objects from
#' the [googledrive](https://googledrive.tidyverse.org) package.
#'
#' @name osf_tbl
#' @aliases osf_tbl_node osf_tbl_file osf_tbl_user
NULL

osf_tbl <- function(x = NULL, subclass = NULL) {
  if (is.list(x) && rlang::is_empty(x)) x <- NULL

  x <- x %||% tibble::tibble(
    name = character(),
    id = character(),
    meta = list()
  )
  new_osf_tbl(x, subclass)
}

new_osf_tbl <- function(x, subclass = NULL) {
  stopifnot(inherits(x, "data.frame"))
  tibble::new_tibble(x, nrow = nrow(x), subclass = c(subclass, "osf_tbl"))
}

# expects a list, where each item is OSF entity represented as a list
as_osf_tbl <- function(x, subclass = NULL) UseMethod("as_osf_tbl")

as_osf_tbl.default <- function(x, subclass = NULL)
  abort("No methods available to coerce this object into an osf_tbl")

# nolint start
as_osf_tbl.data.frame <- function(x, subclass = NULL) new_osf_tbl(x, subclass)
# nolint end

as_osf_tbl.list <- function(x, subclass = NULL) {

  # handle empty lists returned by e.g. .osf_node_children() for childless nodes
  if (rlang::is_empty(x)) return(osf_tbl(subclass = subclass))

  # remove 'data' name that otherwise remains only w/ singleton entities
  x <- unname(x)

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
  out$id   <- as.character(out$id)
  out$name <- as.character(out$name)

  if (subclass == "osf_tbl_node") out$name <- html_decode(out$name)

  new_osf_tbl(out, subclass)
}

#' Rebuild osf_tbl
#'
#' Return an osf_tbl with the proper subclass *if* the input meets all of the
#' requirements for a valid osf_tbl. Otherwise, the object is returned as a
#' standard tibble.
#'
#' @param x an [`osf_tbl`]
#' @return an [`osf_tbl`] or [`tibble`]
#' @noRd
rebuild_osf_tbl <- function(x) {
  if (is_valid_osf_tbl(x)) {
    if (nrow(x) == 0) return(x)
    subclass <- sprintf("osf_tbl_%s", determine_entity_type(x$meta[[1]]))
    return(as_osf_tbl(x, subclass = subclass))
  } else {
    # remove osf_tbl classes
    tibble::new_tibble(x, nrow = nrow(x))
  }
}
