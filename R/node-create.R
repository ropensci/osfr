#' Create a new OSF project or component
#'
#' @param id GUID of the existing OSF project or component in which the new
#'   component will be created
#' @param title,description Set a title (required) and, optionally, a description
#' @param private Boolean, should it be private (defaults to \code{TRUE})
#'
#' @return An object of class \code{osf_tbl_node} with one row per item.
#' @name node-create
#'
#' @examples
#' \dontrun{
#' # create a new project
#' proj <- osf_project(title = "New Private OSF Project")
#'
#' # update/retrieve project details
#' osf_project(proj$id)
#' }
NULL

#' @export
#' @rdname node-create
osf_project_create <- function(title = NULL, description = '', private = TRUE) {
  path <- osf_path("nodes/")
  out <- node_create(path, title, description, private)
  as_osf_tbl_node(out['data'])
}

#' @export
#' @rdname node-create
osf_component_create <- function(id, title = NULL, description = '', private = TRUE) {
  if (missing(id)) stop("Must specify ID of a parent project")
  path <- osf_path(sprintf("nodes/%s/children/", as_id(id)))
  out <- node_create(path, title, description, private)
  as_osf_tbl_node(out['data'])
}


# Create a new node
node_create <- function(
  path,
  title = NULL,
  description = '',
  private = TRUE) {

  if (is.null(title)) stop("Must specify a title")

  body <- list(
    data = list(
      type = "nodes",
      attributes = list(
        title = title,
        category = "project",
        description = description,
        public = !private
      )
    )
  )

  res <- .osf_request("post", path, body = body, encode = "json")
  out <- process_response(res)
  raise_error(out)
  out
}

