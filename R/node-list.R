osf_node_ls <- function(id, n_max = 10) {
  UseMethod("osf_node_ls")
}

osf_node_ls.character <- function(id, n_max = 10) osf_node_ls(as_id(id), n_max)

osf_node_ls.osf_id <- function(id, n_max = 10) {
  out <- switch(id_type(id),
    nodes = .osf_node_children(id, n_max),
    users = .osf_user_nodes(id, n_max),
    abort("The provided ID must correspond to an OSF node or user.")
  )
  as_osf_tbl(out, "osf_tbl_node")
}

osf_node_ls.osf_tbl_node <- function(id, n_max = 10) {
  out <- .osf_node_children(as_id(id), n_max)
  as_osf_tbl(out, "osf_tbl_node")
}

#' @export
osf_node_ls.osf_tbl_user <- function(id, n_max = 10) {
  out <- .osf_user_nodes(as_id(id), n_max)
  as_osf_tbl(out, "osf_tbl_node")
}
