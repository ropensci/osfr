#' List projects or components
#'
#' List the projects or components associated with a user or in the top-level of
#' an OSF project or component.
#'
#' @param x an [`osf_tbl_node`] or [`osf_tbl_user`]
#' @template n_max
#'
#' @return an [`osf_tbl_node`]
#' @examples
#' \dontrun{
#' user <- osf_retrieve_user("me")
#' osf_ls_nodes(user)
#' }
#' @export
osf_ls_nodes <- function(x, n_max = 10) UseMethod("osf_ls_nodes")

#' @export
osf_ls_nodes.osf_tbl_node <- function(x, n_max = 10) {
  x <- make_single(x)
  out <- .osf_node_children(as_id(x), n_max = n_max)
  raise_error(out)
  as_osf_tbl(out, "osf_tbl_node")
}

#' @export
osf_ls_nodes.osf_tbl_user <- function(x, n_max = 10) {
  x <- make_single(x)
  out <- .osf_user_nodes(as_id(x), n_max = n_max)
  raise_error(out)
  as_osf_tbl(out, "osf_tbl_node")
}
