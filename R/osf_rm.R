#' Delete an entity from OSF
#'
#' @description
#' Use `osf_rm()` to **permanently** delete a project, component, file or
#' directory from OSF, including any uploaded files, wiki content, or comments
#' contained therein. Because this process is **irreversible**, osfr will first
#' open the item in your web browser so you can verify what is about to be
#' deleted before proceeding.
#'
#' If the project or component targeted for deletion contains sub-components,
#' those must be deleted first. Setting `recursive = TRUE` will attempt to
#' remove the hierarchy of sub-components before deleting the top-level entity.
#'
#' *Note: This functionality is limited to contributors with admin-level
#' permissions.*
#'
#' @param x One of the following:
#'   * An [`osf_tbl_node`] with a single OSF project or component.
#'   * An [`osf_tbl_file`] containing a single directory or file.
#' @param recursive Remove all sub-components before deleting the top-level
#'   entity. This only applies when deleting projects or components.
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
osf_rm <-
  function(x,
           recursive = FALSE,
           verbose = FALSE,
           check = TRUE) {
  UseMethod("osf_rm")
}

#' @export
osf_rm.osf_tbl_node <-
  function(x,
           recursive = FALSE,
           verbose = FALSE,
           check = TRUE) {

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
        if (!rm_check(child, "node")) return(invisible())
      }
      .osf_node_delete(child)
      if (verbose) {
        message(sprintf("Deleted component %s", names(child)))
      }

    }
  }

  if (check) {
    if (!rm_check(id, "node")) return(invisible())
  }
  out <- .osf_node_delete(id)
  if (isTRUE(out)) {
    if (verbose) message(sprintf("Deleted node %s", id))
    invisible(TRUE)
  }
}

#' @export
osf_rm.osf_tbl_file <-
  function(x,
           recursive = FALSE,
           verbose = FALSE,
           check = TRUE) {

  x <- make_single(x)
  id <- as_id(x)

  type <- get_meta(x, "attributes", "kind")
  endpoint <- get_meta(x, "links", "delete")

  if (check) {
    if (!rm_check(id, type)) return(invisible())
  }

  res <- .wb_request("delete", crul::url_parse(endpoint)$path)
  if (res$status_code == 204) {
    if (verbose) message(sprintf("Deleted file %s", id))
    return(invisible(TRUE))
  } else if (res$status_code == 404) {
    abort("The specified file is no longer available.")
  } else {
    raise_error(process_response(res))
  }
}


#' Remove check
#' Open the item targeted for deletion on OSF and ask the user to verify they
#' want to proceed.
#' @param id GUID
#' @param type a character describing the entity type (e.g., file, folder)
#' @noRd
rm_check <- function(id, type) {
  osf_open(id)
  question <- sprintf(
    "I just opened %s '%s' in your browser.\nAre you sure you want to PERMANENTLY delete it?",
    type,
    id
  )
  yesno_menu(question)
}
