#' OSF Tibbles
#'
#' Items retrieved from OSF are represented as `osf_tbl` objects, specialized
#' data frames based on the [tibble][tibble::tibble-package] class. See below
#' for additional details.
#'
#' Each row of an `osf_tbl` represents a single OSF entity, which could be a
#' user, project, component, directory, or file. An `osf_tbl` must include
#' the following 3 variables:
#'
#' * `name`: the name or title of the entity
#' * `id`: the unique identifier assigned to the entity
#' * `meta`: a list-column that stores the processed response returned by
#' OSF's API
#'
#' The `meta` column is primarily intended for use by `osfr`'s functions and
#' should not be altered by users.
#'
#' @section Subclasses:
#'
#' An `osf_tbl` is the parent class of 3 subclasses that are used to represent
#' each of OSF's main entities: `osf_tbl_user` for users, `osf_tbl_node` for
#' nodes (i.e., projects and components) and `osf_tbl_file` for files and
#' directories.
#'
#' @section Acknowledgements:
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

#' @export
rbind.osf_tbl <- function(..., deparse.level = 1)  {
  out <- base::rbind.data.frame(..., deparse.level = deparse.level)
  rebuild_osf_tbl(out)
}

#' Validate and rebuild osf_tbl
#'
#' Return an osf_tbl with the proper subclass if the input meets all of the
#' requirements to be a valid osf_tbl. Otherwise, strip the osf_tbl class
#' and subclass before returning.
#' @noRd
rebuild_osf_tbl <- function(x) {
  if (is.data.frame(x) &&
      has_osf_tbl_colnames(x) &&
      has_osf_tbl_coltypes(x) &&
      has_uniform_entity_type(x)) {
    subclass <- sprintf("osf_tbl_%s", determine_entity_type(x$meta[[1]]))
    return(as_osf_tbl(x, subclass = subclass))
  } else {
    # remove osf_tbl classes
    classes <- class(x)[!grepl("osf_tbl", class(x), fixed = TRUE)]
    structure(x, class = classes)
  }
}

#' Check the data frame contains the required columns
#' @return logical
#' @noRd
has_osf_tbl_colnames <- function(x) {
  required <- c("name", "id", "meta")
  missing <- setdiff(required, colnames(x))
  is_empty(missing)
}

#' Check that required columns are the correct type
#' @return logical
#' @noRd
has_osf_tbl_coltypes <- function(x) {
  expected <- c(name = "character", id = "character", meta = "list")
  found <- vapply(x[names(expected)], FUN = typeof, FUN.VALUE = character(1L))
  incorrect <- found[expected != found]
  is_empty(incorrect)
}

#' Check that all rows are of the same OSF entity type
#' @return logical
#' @noRd
has_uniform_entity_type <- function(x) {
  types <- vapply(x$meta, determine_entity_type, FUN.VALUE = character(1))
  length(unique(types)) == 1
}

#' Determine OSF entity list type
#'
#' @param x OSF entity list containing links, relationships, and attributes
#'   slots
#' @return scalar character vector: "user", "node", or "file"
#' @noRd

determine_entity_type <- function(x) {
  stopifnot(is.list(x) && rlang::is_named(x))
  required_slots <- c("attributes", "links", "attributes")
  stopifnot(all(required_slots %in% names(x)))

  test_slots <- c(user = "family_name", node = "category", file = "kind")
  matched <- test_slots %in% names(x$attributes)
  if (sum(matched) == 1) {
    names(test_slots)[matched]
  } else {
    abort("Could not determine `x`'s entity type.")
  }
}
