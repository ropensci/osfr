# OSF API endpoints -------------------------------------------------------

#' Create a new project or component
#' @param id a GUID for an existing project or component. If defined, the
#'   corresponding node will serve as the parent for the new subcomponent. If
#'   left undefined a top-level project will be created instead.
#' @param title Required, title for the new node
#' @param description Optional, description for the new node
#' @param public Logical, should the new node be publicly available (`TRUE`) or
#'   private (`FALSE`)
#' @param category One of the pre-defined node categories.
#' @references https://developer.osf.io/#operation/nodes_create
#' @noRd
.osf_node_create <-
  function(id = NULL,
           title,
           description = NULL,
           public = FALSE,
           category = NULL) {

  if (missing(title)) abort("A title must be provided.")
  if (!is.null(category)) category <- check_category(category)

  path <- "nodes/"
  if (!is.null(id)) {
    path <- url_path(path, sprintf("%s/children/", id))
  }

  body <- list(
    data = list(
      type = "nodes",
      attributes = list(
        title = title,
        category = category %||% "",
        description = description %||% "",
        public = public
      )
    )
  )

  res <- .osf_request("post", path, body = body, encode = "json")
  out <- process_response(res)
  raise_error(out)
  out
}

#' Retrieves the details of a given node (project or component)
#' @param id a node's GUID
#' @references
#' https://developer.osf.io/#operation/nodes_read
#' @noRd
.osf_node_retrieve <- function(id) {
  path <- sprintf("nodes/%s/", id)
  res <- .osf_request("get", path)
  process_response(res)
}

#' Retrieves the details for a given user
#' @param id a user's GUID
#' @references
#' https://developer.osf.io/#operation/users_read
#' @noRd
.osf_user_retrieve <- function(id) {
  path <- sprintf("/users/%s/", id)
  res <- .osf_request("get", path)
  process_response(res)
}

#' Retrieve the details of a file or folder
#' @param id a file's GUID or a Waterbutler ID
#' @references
#' https://developer.osf.io/#operation/files_detail
#' @noRd
.osf_file_retrieve <- function(id) {
  path <- sprintf("files/%s/", id)
  res <- .osf_request("get", path)
  process_response(res)
}

#' Permanently delete a node
#' @param id a node's GUID
#' @return Since this endpoint doesn't return any useful information we return
#'   `TRUE` if deletion was successful and the JSON error message otherwise
#' @references
#' https://developer.osf.io/#operation/nodes_delete
#' @noRd
.osf_node_delete <- function(id) {
  path <- sprintf("nodes/%s/", id)
  res <- .osf_request("delete", path)
  if (res$status_code == 204) return(TRUE)
  raise_error(process_response(res))
}


# Paginated endpoints -----------------------------------------------------
# NOTE: .osf_paginated_request() calls raise_error() internally

#' List the child nodes of a parent project or component
#' @param id a node's GUID
#' @param n_max max number of pages to retrieve from the API
#' @param query a list of query params to include in the call. This is necessary
#'   for filtering.
#' @param verbose Logical passed to [`.osf_paginated_request()`]
#' @return An entity collection with entities sorted by `date_modified`
#' @references
#' https://developer.osf.io/#operation/nodes_list
#' @noRd
.osf_node_children <- function(id, n_max, query = list(), verbose = FALSE) {
  path <- sprintf("nodes/%s/children/", id)
  .osf_paginated_request("get", path, query, n_max = n_max, verbose = verbose)
}

#' List the nodes a user is a contributor to
#' @param id a user's GUID
#' @inheritParams .osf_node_children
#' @return An entity collection with entities sorted by `date_modified`
#' @references
#' https://developer.osf.io/#operation/users_nodes_list
#' @noRd
.osf_user_nodes <- function(id, n_max, query = list(), verbose = FALSE) {
  path <- sprintf("users/%s/nodes/", id)
  .osf_paginated_request("get", path, query, n_max = n_max, verbose = verbose)
}
