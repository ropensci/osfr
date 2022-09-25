#' Create a new project or component on OSF
#'
#' @description
#' Use `osf_create_project()` to create a new top-level project on OSF. A nested
#' component can be created by providing an [`osf_tbl_node`] containing an
#' existing project or component to `osf_create_component()`'s `x` argument.
#'
#' @inheritSection osf_tbl OSF nodes
#'
#' @references
#' 1. OSF Guides: Create a Project.
#' <https://help.osf.io/article/383-creating-a-project>.
#' 2. OSF Guides: Create a Component.
#' <https://help.osf.io/article/253-create-components>.
#'
#' @param x An [`osf_tbl_node`] with a single OSF project or component that will
#'   serve as the new sub-component's parent node.
#' @param title,description Set a title (required) and, optionally, a
#'   description.
#' @param public Logical, should it be publicly available (`TRUE`) or private
#'   (`FALSE`, the default)?
#' @param category Character string, specify a category to change the icon
#'   displayed on OSF. The defaults are `"project"` for projects and
#'   `"uncategorized"` for components. The specified category can be easily
#'   changed later on OSF. Valid category options include:
#' * analysis
#' * communication
#' * data
#' * hypothesis
#' * instrumentation
#' * methods and measures
#' * procedure
#' * project
#' * software
#' * other
#'
#' @return An [`osf_tbl_node`] containing the new project or component.
#'
#' @examples
#' \dontrun{
#' # create a new public project
#' project <- osf_create_project(title = "Private OSF Project", public = TRUE)
#'
#' # add a private component to the new project
#' component <- osf_create_component(project, title = "Project Data")
#' }
#' @name osf_create
NULL

#' @export
#' @rdname osf_create
osf_create_project <-
  function(title,
           description = NULL,
           public = FALSE,
           category = "project") {

  if (missing(title)) abort("Must define a title for the new project.")

  out <- .osf_node_create(
    title = title,
    description = description,
    public = public,
    category = category
  )
  as_osf_tbl(out["data"], "osf_tbl_node")
}

#' @export
#' @rdname osf_create
osf_create_component <-
  function(x,
           title,
           description = NULL,
           public = FALSE,
           category = NULL) {

  if (missing(x) || !inherits(x, "osf_tbl_node"))
    abort("`x` must be an `osf_tbl_node` referencing an existing project/component.")
  if (missing(title)) abort("Must define a title for the new component.")

  out <- .osf_node_create(
    id = as_id(x),
    title = title,
    description = description,
    public = public,
    category = category
  )
  as_osf_tbl(out["data"], "osf_tbl_node")
}
