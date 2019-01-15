#' Retrieve an entity from OSF
#'
#' Create an [`osf_tbl`] for an existing OSF project, component, file, or user
#' based on the associated unique identifier. Usually this is a 5-character
#' global unique identifier (GUID) but for files or directories, it could also
#' be a 24-character Waterbutler ID.
#'
#' To begin using osfr to interact with resources on OSF you must use one of the
#' following *retrieve* functions to create an [`osf_tbl`] that contains
#' the entity interest. Note the functions are entity-type specific, use:
#' * `osf_retrieve_node()` to retrieve a project or component
#' * `osf_retrieve_file()` to retrieve a file or directory
#' * `osf_retrieve_user()` to retrieve a user
#'
#' @section A note on 3rd-party storage providers:
#' While OSF supports integration with a variety of 3rd-party cloud storage
#' providers, osfr can currently only access files stored on the default OSF
#' storage service. Support for additional storage providers is planned for a
#' future release.
#'
#' @param id An OSF identifier corresponding to an OSF user, project, component,
#'   or file. Set `id = "me"` to retrieve your own OSF profile.
#' @return An [`osf_tbl_user`], [`osf_tbl_node`], or [`osf_tbl_file`] containing
#'   the corresponding OSF entity.
#'
#' @examples
#' \dontrun{
#'  # retrieve your own OSF user profile (must be authenticated, ?osf_auth)
#'  osf_retrieve_user("me")
#'
#' # retrieve the Psychology Reproducibility Project (P:RP, osf.io/ezum7)
#' osf_retrieve_node("ezum7")
#'
#' # get the first figure from the P:RP
#' osf_retrieve_file("https://osf.io/7js8c")
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

  # prevent file retrieval from other providers until properly supported
  if (out$data$attributes$provider != "osfstorage") {
    abort(paste0(
      "3rd party storage add-ons are not currently supported\n",
      "* The requested file is stored on ", out$data$attributes$provider, "\n"
    ))
  }

  as_osf_tbl(out["data"], "osf_tbl_file")
}
