#' Create a new project or component on OSF
#'
#' Use `osf_create_project()` to create a new top-level project on OSF. A nested
#' component can be created by providing an [`osf_tbl_node`] referencing an
#' existing project or component to `osf_create_component()`'s `x` argument.
#'
#' @section: OSF Nodes
#' Both projects and components are implemented as *nodes* within OSF, which
#' explains why they are functionally identical. The only distinction between
#' the two is projects exist at the top-level of an organizational hierarchy,
#' and components are nested within a parent project or another component. As
#' such, osfr uses the same S3 class to represent both projects and components:
#' an [`osf_tbl_node`].
#'
#' @references
#' 1. OSF Guides: Create a Project.
#' <http://help.osf.io/m/projects/l/481539-create-a-project>.
#' 2. OSF Guides: Create a Component.
#' <http://help.osf.io/m/projects/l/481998-create-components>
#'
#' @param x An [`osf_tbl_node`] with a single OSF project or component that will
#'   serve as the new sub-component's parent node.
#' @param title,description Set a title (required) and, optionally, a
#'   description.
#' @param public Logical, should it be publicly available (`TRUE`) or private
#'   (`FALSE`, the default)?
#'
#' @return an [`osf_tbl_node`] containing the new OSF project or component
#'
#' @examples
#' \dontrun{
#' # create a new private project
#' project <- osf_create_project(title = "Private OSF Project")
#'
#' # create a new component
#' component <- osf_create_component(project, title = "Project Data")
#' }
#' @name osf_create
NULL

#' @export
#' @rdname osf_create
osf_create_project <- function(title, description = NULL, public = FALSE) {
  if (missing(title)) abort("Must define a title for the new project.")

  out <- .osf_node_create(
    title = title,
    description = description,
    public = public
  )
  as_osf_tbl(out["data"], "osf_tbl_node")
}

#' @export
#' @rdname osf_create
osf_create_component <- function(x, title, description = NULL, public = FALSE) {
  if (missing(x) || !inherits(x, "osf_tbl_node"))
    abort("`x` must be an `osf_tbl_node` referencing an existing project/component. ")
  if (missing(title)) abort("Must define a title for the new component.")

  out <- .osf_node_create(
    id = as_id(x),
    title = title,
    description = description,
    public = public
  )
  as_osf_tbl(out["data"], "osf_tbl_node")
}
