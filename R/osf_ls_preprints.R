#' List preprints from a specific user
#'
#' List the preprints that are associated with a specific user.
#' Note that will return *all* preprints, unlike [osf_ls_nodes()].
#'
#' @param x one of the following:
#'   * An [`osf_tbl_user`] with a single OSF user.
#' @template filter-pattern
#' @template n_max
#' @template verbose
#'
#' @return An [`osf_tbl_node`] with one row for each OSF preprint
#'   ordered by modification time.
#' @examples
#' \dontrun{
#' # List your recent projects and components
#' osf_retrieve_user("me") %>%
#'   osf_ls_preprints()
#' }
#' @seealso [`osf_ls_nodes()`] to generate a list of all project components.
#' @export
osf_ls_preprints <-
  function(x,
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {
  UseMethod("osf_ls_preprints")
}

#' @export
osf_ls_preprints.osf_tbl_user <-
  function(x,
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {

  x <- make_single(x)

  out <- .osf_user_preprints(
    id = as_id(x),
    n_max = n_max,
    query = filter_nodes(pattern = pattern),
    verbose = verbose
  )

  raise_error(out)
  as_osf_tbl(out, "osf_tbl_node")
}
