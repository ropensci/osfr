#' Retrieve an entity from OSF based on its identifier
#'
#' Use `osf_retrieve()` to create an `osf_tbl` for an existing OSF entity based
#' on its unique identifier. Usually this is a 5-character global unique
#' identifier (GUID) but for files or directories could be a 24-character
#' Waterbutler ID.
#'
#' @param id An OSF identifier corresponding to an OSF user, project, component,
#'   or file. Set `id = "me"` to retrieve your own OSF profile.
#' @return an [`osf_tbl_user`], [`osf_tbl_node`], or [`osf_tbl_file`] containing
#'   the corresponding OSF entity
#' @examples
#' \dontrun{
#'  osf_retrieve_user("me")
#' }
#' @name osf_retrieve
NULL

#' @export
#' @rdname osf_retrieve
osf_retrieve_user <- function(id) {
  stopifnot(is.character(id))
  id <- make_single(id)

  # accommodate special case of 'me' id
  id <- switch(id, me = structure(id, class = "osf_id"), as_id(id))

  out <- .osf_user_retrieve(id)
  raise_error(out)

  as_osf_tbl(out["data"], "osf_tbl_user")
}

#' @export
#' @rdname osf_retrieve
osf_retrieve_node <- function(id) {
  stopifnot(is.character(id))
  id <- make_single(id)
  id <- as_id(id)

  out <- .osf_node_retrieve(id)
  raise_error(out)

  as_osf_tbl(out["data"], "osf_tbl_node")
}

#' @export
#' @rdname osf_retrieve
osf_retrieve_file <- function(id) {
  stopifnot(is.character(id))
  id <- make_single(id)
  id <- as_id(id)

  out <- .osf_file_retrieve(id)
  raise_error(out)

  as_osf_tbl(out["data"], "osf_tbl_file")
}
