#' Create a new OSF project or component
#'
#' Both projects and components can be created with `osf_node_create()`. To
#' create a new top-level project leave `x = NULL`. To create a new component
#' use `x` to specify the existing parent project or component in which the new
#' component will be created.
#'
#' @section: OSF Nodes:
#' On OSF, projects and components are both implemented as *nodes* within the
#' system; this explains why they are functionally identical on
#' <https://www.osf.io>. The only distinction between the two is projects exist
#' at the top-level of an organizational hierarchy and components are
#' sub-projects within a parent project. As such, the same suite of `osfr`
#' functions can be used to manage both projects and components.
#'
#' @template input-osf-node
#' @param title,description Set a title (required) and, optionally, a description
#' @param private Boolean, should it be private (defaults to `TRUE``)
#'
#' @return an [`osf_tbl_node`]
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
osf_project_create <- function(title = NULL, description = "", private = TRUE) {
  path <- osf_path("nodes/")
  out <- node_create(path, title, description, private)
  as_osf_tbl(out["data"], "osf_tbl_node")
}

#' @export
#' @rdname node-create
osf_component_create <- function(x, title = NULL, description = "", private = TRUE) {
  if (missing(x)) stop("Must specify ID of a parent project")
  path <- osf_path(sprintf("nodes/%s/children/", as_id(x)))
  out <- node_create(path, title, description, private)
  as_osf_tbl(out["data"], "osf_tbl_node")
}


# Create a new node
node_create <- function(
  path,
  title = NULL,
  description = "",
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
