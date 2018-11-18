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
#' @examples
#' \dontrun{
#' delete_project(id = "y7w8p")}
#' @export
delete_project <- function(id, recursive = FALSE) {
  if (recursive) {
    child_ids <- recurse_node(id)

    # reverse to begin with the most deeply nested node
    for (i in rev(seq_along(child_ids))) {
      child <- child_ids[i]
      message(sprintf("Deleting %s", names(child)))
      if (child == id) break
      delete_node(child)
      message(sprintf("Deleted subcomponent %s", names(child)))
    }
  }
  delete_node(id)
}

#' @rdname delete_project
#' @export
delete_component <- function(id, recursive = FALSE) {
  delete_project(id, recursive)
}

# Delete a single node
delete_node <- function(id) {
  if (missing(id)) stop("Must specify a node identifier")
  cli <- osf_cli()
  path <- osf_path(sprintf('nodes/%s/', id))
  res <- cli$delete(path)

  if (res$status_code == 204) return(invisible(id))

  # Print error codes
  out <- jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
  http_error(res$status_code, out$errors[[1]]$detail)
}
