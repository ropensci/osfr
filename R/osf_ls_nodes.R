#' List projects or components on OSF
#'
#' List the projects or components associated with a user or contained in the
#' top-level of another OSF project or component.
#'
#' @param x one of the following:
#'   * An [`osf_tbl_node`] with a single project or component.
#'   * An [`osf_tbl_user`] with a single OSF user.
#' @template filter-pattern
#' @template n_max
#' @template verbose
#'
#' @return An [`osf_tbl_node`] with one row for each OSF project or component,
#'   ordered by modification time.
#' @examples
#' \dontrun{
#' # List your recent projects and components
#' osf_retrieve_user("me") %>%
#'   osf_ls_nodes()
#'
#' # List the first 10 components in the #ScanAllFish project
#' fish_ctscans <- osf_retrieve_node("ecmz4")
#' osf_ls_nodes(fish_ctscans)
#'
#' # Now just the components with scans of species from the Sphyrna genus
#' osf_ls_nodes(fish_ctscans, pattern = "Sphyrna")
#' }
#' @seealso [`osf_ls_files()`] to generate a list of files and files.
#' @export
osf_ls_nodes <-
  function(x,
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {
  UseMethod("osf_ls_nodes")
}

#' @export
osf_ls_nodes.osf_tbl_node <-
  function(x,
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {
  x <- make_single(x)

  out <- .osf_node_children(
    id = as_id(x),
    n_max = n_max,
    query = filter_nodes(pattern = html_encode(pattern)),
    verbose = verbose
  )

  raise_error(out)
  as_osf_tbl(out, "osf_tbl_node")
}

#' @export
osf_ls_nodes.osf_tbl_user <-
  function(x,
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {
  x <- make_single(x)

  out <- .osf_user_nodes(
    id = as_id(x),
    n_max = n_max,
    query = filter_nodes(pattern = pattern),
    verbose = verbose
  )

  raise_error(out)
  as_osf_tbl(out, "osf_tbl_node")
}
