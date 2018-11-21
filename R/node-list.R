# List all node children
# @param id, a node ID, user ID, osf_tbl_node or osf_tbl_user
# @returns osf_tbl_node
#' @export
osf_node_ls <- function(id, n_max = 10) {
  UseMethod("osf_node_ls")
}

#' @export
osf_node_ls.character <- function(id, n_max = 10) osf_node_ls(as_id(id))

#' @export
osf_node_ls.osf_id <- function(id, n_max = 10) {

  # determine if provided ID maps to a user or node
  out <- try(.osf_node_retrieve(id), silent = TRUE)
  if (inherits(out, "try-error")) {
    out <- .osf_user_retrieve(id)
  }

  out <- switch(out$data$type,
    nodes = as_osf_tbl_node(out['data']),
    users = as_osf_tbl_user(out['data'])
  )
  osf_node_ls(out)
}

#' @export
osf_node_ls.osf_tbl_node <- function(id, n_max = 10) {
  out <- .osf_node_children(as_id(id), n_max)
  as_osf_tbl_node(out)
}

#' @export
osf_node_ls.osf_tbl_user <- function(id, n_max = 10) {
  out <- .osf_user_nodes(as_id(id), n_max)
  as_osf_tbl_node(out)
}

