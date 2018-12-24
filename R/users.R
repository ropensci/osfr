#' Retrieve an entity from OSF based on its identifier
#'
#' Use `osf_retrieve()` to create an `osf_tbl` for an existing OSF entity.
#'
#' @param id An OSF identifier corresponding to an OSF user, project, component,
#'   or file.
#' @return an `osf_tbl_user`, `osf_tbl_node`, or `osf_tbl_file`
#' @examples
#' \dontrun{
#'  osf_retrieve("dguxh")
#' }
#' @export

osf_retrieve <- function(id) {
  id <- as_id(id)

  if (length(id) > 1) {
    warn(sprintf("Retrieving only the first ID of %i", length(id)))
  }

  type <- id_type(id)
  subclass <- paste0("osf_tbl_", sub("s$", "", type))

  out <- switch(
    type,
    nodes = .osf_node_retrieve(id),
    users = .osf_user_retrieve(id),
    files = .osf_file_retrieve(id)
  )
  raise_error(out)

  as_osf_tbl(out['data'], subclass)
}
