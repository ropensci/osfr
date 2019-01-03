#' OSF Delete
#'
#' Use `osf_rm()`  to *permanently* delete a project or component from OSF,
#' including any uploaded files, wiki content, or comments contained therein.
#' This process does not request confirmation, so please **handle with
#' care**. This functionality is limited to contributors with admin-level
#' permissions.
#'
#' If the project or component contains sub-components, those must be deleted
#' first. Setting `recursive = TRUE` will attempt to remove the hierarchy
#' of sub-components before deleting the top-level entity.
#'
#' @param x an [`osf_tbl_node`]
#' @param recursive remove sub-components before deleting the top-level entity
#' @template verbose
#'
#' @return Invisibly returns `TRUE` if deletion was successful.
#'
#' @examples
#' \dontrun{
#' project <- osf_create_project("My Short-Lived Project")
#' osf_rm(project)
#' }
#'
#' @export
osf_rm <- function(x, recursive = FALSE, verbose = FALSE) {
  UseMethod("osf_rm")
}

#' @export
osf_rm.osf_tbl_node <- function(x, recursive = FALSE, verbose = FALSE) {
  x <- make_single(x)
  id <- as_id(x)

  if (recursive) {
    child_ids <- recurse_node(id)
    if (verbose) {
      message(
        sprintf("Retrieved %i child nodes under %s", length(child_ids), id))
    }

    # reverse to begin with the most deeply nested node
    for (i in rev(seq_along(child_ids))) {
      child <- child_ids[i]
      message(sprintf("Deleting %s", names(child)))
      if (child == id) break
      .osf_node_delete(child)
      if (verbose) {
        message(sprintf("Deleted subcomponent %s", names(child)))
      }

    }
  }

  out <- .osf_node_delete(id)
  if (isTRUE(out)) {
    if (verbose) message(sprintf("Deleted node %s", id))
    invisible(TRUE)
  }
}
