#' Create a component within a project
#'
#' This function creates a new component within a project. New components can
#' be created as private or public components using the \code{private} argument.
#'
#' @param id OSF id (osf.io/XXXXX; just XXXXX) of parent project
#' @param title Title of the component [required]
#' @param description Description of the component [optional]
#' @param category Category of component, for valid categories
#'   see \code{\link{process_category}}, defaults to `project`
#' @param private Boolean of whether the component is supposed to be private
#'
#' @return OSF id of created component
#'
#' @export
#' @seealso \code{\link{osf_project}}
#' @examples
#' \dontrun{
#' create_component("12345", "New Test Component")
#' }

create_component <- function(
  id,
  title,
  description = '',
  category = 'project',
  private = TRUE) {

  if (missing(id)) stop("Specify ID of a parent project")
  if (missing(title)) stop("Specify a component title")
  process_category(category)
  path <- osf_path(sprintf('nodes/%s/children/', id))

  out <- create_node(path, title, description, private)
  out$data$id
}


#' Update a component on the OSF
#'
#' This function updates the private/public status of a component. By default,
#' the function turns a private component into a public component. This
#' function simply wraps the \code{\link{update_project}} function
#' as it has the same operations.
#'
#' @param id OSF id (osf.io/XXXXX; just XXXXX)
#' @param private Set project to private/public (default changes to public)
#'
#' @return Boolean, update success
#' @export
#'
#' @seealso \code{\link{update_project}}
#' @examples
#' \dontrun{
#' update_component("12345")
#' }

update_component <- function(id, title = NULL, description = NULL, private = NULL) {
  out <- update_node(id, title, description, private)
  out$data$id
}
