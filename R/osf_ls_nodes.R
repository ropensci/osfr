#' List projects or components on OSF
#'
#' List the projects or components associated with a user or contained in the
#' top-level of another OSF project or component.
#'
#' @param x one of the following:
#'   * an [`osf_tbl_node`] with a single project or component.
#'   * an [`osf_tbl_user`] with a single OSF user.
#' @template filter-pattern
#' @template n_max
#'
#' @return an [`osf_tbl_node`]
#' @examples
#' \dontrun{
#' # List your recent projects and components
#' user <- osf_retrieve_user("me")
#' osf_ls_nodes(user)
#' }
#' @seealso [`osf_ls_files()`] to generate a list of files and files
#' @export
osf_ls_nodes <- function(x, pattern = NULL, n_max = 10) UseMethod("osf_ls_nodes")

#' @export
osf_ls_nodes.osf_tbl_node <- function(x, pattern = NULL, n_max = 10) {
  x <- make_single(x)
  out <- .osf_node_children(as_id(x), n_max, filter_nodes(pattern = pattern))
  raise_error(out)
  as_osf_tbl(out, "osf_tbl_node")
}

#' @export
osf_ls_nodes.osf_tbl_user <- function(x, pattern = NULL, n_max = 10) {
  x <- make_single(x)
  out <- .osf_user_nodes(as_id(x), n_max, filter_nodes(pattern = pattern))
  raise_error(out)
  as_osf_tbl(out, "osf_tbl_node")
}
