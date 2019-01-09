# OSF API endpoints -------------------------------------------------------

#' Create a new project or component
#' @param id GUID for an existing OSF project or component. If defined, the
#'   corresponding node will serve as the parent for the new subcomponent. If
#'   left undefined a top-level project will be created instead.
#' @param title Required, title for the new node
#' @param description Optional, description for the new node
#' @param public Logical, should the new node be publicly available (`TRUE`) or
#'   private (`FALSE`)
#' @noRd
.osf_node_create <- function(id = NULL, title, description = NULL, public = FALSE) {
  if (missing(title)) abort("A title must be provided.")

  if (is.null(id)) {
    path <- osf_path("nodes/")
  } else {
    path <- osf_path(sprintf("nodes/%s/children/", id))
  }

  body <- list(
    data = list(
      type = "nodes",
      attributes = list(
        title = title,
        category = "project",
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

# e.g., .osf_node_retrieve("k35ut)
.osf_node_retrieve <- function(id) {
  res <- .osf_request("get", osf_path(sprintf("nodes/%s/", id)))
  process_response(res)
}

# e.g., .osf_node_delete("k35ut)
.osf_node_delete <- function(id) {
  path <- osf_path(sprintf("nodes/%s/", id))
  res <- .osf_request("delete", path)

  # since this endpoint doesn't return any useful info we'll return TRUE if
  # successful or the error message if not
  if (res$status_code == 204) return(TRUE)
  raise_error(process_response(res))
}

# list all child nodes
.osf_node_children <- function(id, n_max, query = list(), verbose = FALSE) {
  path <- osf_path(sprintf("nodes/%s/children/", id))
  .osf_paginated_request("get", path, query, n_max = n_max, verbose = verbose)
}

# retrieve user info
.osf_user_retrieve <- function(id) {
  path <- osf_path(sprintf("/users/%s/", id))
  res <- .osf_request("get", path)
  process_response(res)
}


# list user's nodes
.osf_user_nodes <- function(id, n_max, query = list(), verbose = FALSE) {
  path <- osf_path(sprintf("users/%s/nodes/", id))
  .osf_paginated_request("get", path, query, n_max = n_max, verbose = verbose)
}

# e.g., .osf_file_retrieve("5be5e1fdfe3eca00188178c3")
.osf_file_retrieve <- function(id) {
  res <- .osf_request("get", osf_path(sprintf("files/%s/", id)))
  process_response(res)
}
