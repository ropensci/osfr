#' Update an OSF project or component
#'
#' Update the title or description of a project or component. You can also
#' change the privacy settings from public to private (or vice versa).
#'
#' @param id OSF id (osf.io/XXXXX; just XXXXX)
#' @param title,description Change the title or description
#' @param private Change privacy settings to private or public
#'
#' @return \code{osf_tbl_node}
#' @name node-update
#'
#' @examples
#' \dontrun{
#' proj <- osf_project_create("temporary-title", private = TRUE)
#' proj <- osf_project_update(proj$id, title = "Final Title", private = FALSE)
#' }
NULL

#' @export
#' @rdname node-update
osf_project_update <- function(id,
                               title = NULL,
                               description = NULL,
                               private = NULL) {
  if (missing(id)) stop("Must specify ID of a project to update")
  out <- node_update(id, title, description, private)
  as_osf_tbl_node(out['data'])
}

#' @export
#' @rdname node-update
osf_component_update <- function(id,
                                 title = NULL,
                                 description = NULL,
                                 private = NULL) {
  if (missing(id)) stop("Must specify ID of a component to update")
  out <- node_update(id, title, description, private)
  as_osf_tbl_node(out['data'])
}


node_update <- function(
  id,
  title = NULL,
  description = NULL,
  private = NULL) {
  id <- as_id(id)
  public <- if (is.logical(private)) !private else NULL

  body <- list(
    data = list(
      type = "nodes",
      id = as.character(id),
      attributes = list()
    )
  )

  attrs <- list(title = title, description = description, public = public)
  body$data$attributes <- modifyList(body$data$attributes, attrs)

  if (length(body$data$attributes) == 0) {
    stop("No updated attribute values specified")
  }

  path <- osf_path(sprintf('nodes/%s/', id))
  res <- .osf_request("patch", path, body = body, encode = "json")
  res$raise_for_status()
  process_response(res)
}
