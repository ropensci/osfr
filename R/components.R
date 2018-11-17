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
