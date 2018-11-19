#' Delete a project or component
#'
#' Functions to \strong{permanently} delete a project or component from OSF,
#' including any uploaded files, wiki content, or comments contained therein.
#' This process does not request confirmation, so please \strong{handle with
#' care}. This functionality is limited to contributors with admin-level
#' permissions.
#'
#' If the project or component contains sub-components, those must be deleted
#' first. Setting \code{recursive = TRUE} will attempt to remove the hierarchy
#' of sub-components before deleting the top-level entity.
#'
#' @param id the OSF entity's unique identifier
#' @param recursive remove sub-components before deleting the top-level entity
#'
#' @return the deleted entity's unique identifier (invisibly)
#' @name node-delete
#' @examples
#' \dontrun{
#' delete_project(id = "y7w8p")}
NULL

#' @rdname node-delete
#' @export
osf_project_delete <- function(id = NULL, recursive = FALSE, verbose = FALSE) {
  node_delete(id, recursive)

}

#' @rdname node-delete
#' @export
osf_component_delete <- function(id = NULL, recursive = FALSE, verbose = FALSE) {
  node_delete(id, recursive)
}

# Delete a single node
node_delete <- function(id, recursive = FALSE, verbose = FALSE) {
  if (is.null(id)) stop("Must specify a node identifier")
  id <- as_id(id)

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
    return(TRUE)
  }
}
