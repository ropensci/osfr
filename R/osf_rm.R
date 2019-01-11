#' Delete projects or components from OSF
#'
#' @description
#' Use `osf_rm()` to *permanently* delete a project or component from OSF,
#' including any uploaded files, wiki content, or comments contained therein.
#' Because this process is irreversible, osfr will first open the item in your
#' web browser so you can verify the item before proceeding.
#'
#' If the project or component contains sub-components, those must be deleted
#' first. Setting `recursive = TRUE` will attempt to remove the hierarchy
#' of sub-components before deleting the top-level entity.
#'
#' *Note: This functionality is limited to contributors with admin-level
#' permissions.*
#'
#' @param x an [`osf_tbl_node`]
#' @param recursive Remove all sub-components before deleting the top-level entity.
#' @param check If `FALSE` deletion will proceed without opening the item or
#'   requesting verification---this effectively removes your safety net.
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
osf_rm <- function(x, recursive = FALSE, verbose = FALSE, check = TRUE) {
  UseMethod("osf_rm")
}

#' @export
osf_rm.osf_tbl_node <- function(x, recursive = FALSE, verbose = FALSE, check = TRUE) {
  x <- make_single(x)
  id <- as_id(x)

  if (recursive) {
    child_ids <- recurse_node(id, maxdepth = Inf)
    if (verbose) {
      message(
        sprintf("Retrieved %i components under node: %s", length(child_ids), id))
    }

    # reverse to begin with the most deeply nested node
    for (i in rev(seq_along(child_ids))) {
      child <- child_ids[i]
      if (child == id) break
      if (check) {
        if (!rm_check(child)) return(invisible())
      }
      .osf_node_delete(child)
      if (verbose) {
        message(sprintf("Deleted component %s", names(child)))
      }

    }
  }

  if (check) {
    if (!rm_check(id)) return(invisible())
  }
  out <- .osf_node_delete(id)
  if (isTRUE(out)) {
    if (verbose) message(sprintf("Deleted node %s", id))
    invisible(TRUE)
  }
}

rm_check <- function(id) {
  osf_open(id)
  question <- sprintf(
    "I just opened node '%s' in your browser.\nAre you sure you want to PERMANENTLY delete it?",
    id
  )
  yesno_menu(question)
}
