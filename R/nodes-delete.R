#' Delete a project or component
#'
#' Functions to \strong{permanently} delete a project or component from OSF,
#' including any uploaded files, wiki content, or comments contained therein. NOTE: this
#' process does not request confirmation, so please handle with care. If you do
#' not have backups of the files, it is easy to lose everything (in the
#' component).
#'
#' @section: Recursion
#' A node cannot be deleted if it contains any sub-components.
#'
#' @section: Permissions
#' Deletion is limited to users with admin-level rights.
#'
#' @param id OSF id (osf.io/XXXXX)
#' @param recursive should child nodes be deleted recursively?
#'
#' @section Projects:
#' If a project contains any components, those must be deleted first before proceeding.
#'
#' However, if I a project contains any files they will not prevent deletion.
#'
#' @section Components
#'
#'
#' @name delete
#' @return Boolean, delete success
#' @examples
#' \dontrun{
#' delete_project(id = "12345")}
NULL

#' @export
#' @rdname delete
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

#' @export
#' @rdname delete
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
