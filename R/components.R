#' Create a component within a project
#'
#' @param id OSF id (osf.io/XXXX; just XXXX) of parent project
#' @param title Title of the component [required]
#' @param description Description of the component [optional]
#' @param category Category of component, for valid categories
#'   see \code{\link{process_category}}, defaults to `project`
#' @param private Boolean of whether the component is supposed to be private
#'
#' @return OSF id of created component
#'
#' @export
#' @seealso \code{\link{create_project}}

create_component <- function(
  id,
  title,
  description = '',
  category = 'project',
  private = TRUE) {

  process_category(category)
  config <- get_config(TRUE)

  url_osf <- construct_link(sprintf('nodes/%s/children/', id))

  body <- list(
    data = list(
      type = 'nodes',
      attributes = list(
        title = title,
        category = category,
        description = description,
        public = (!private)
      )
    )
  )

  call <- httr::POST(
    url = url_osf,
    body = body, encode = 'json',
    config)

  if (call$status_code != 201) {
  	stop('Failed to create new component')
  }

  res <- process_json(call)
  id <- res$data$id

  return(id)
}


#' Update a component on the OSF
#'
#' Simply wraps the \code{\link{update_project}} function
#' because it has the same operations.
#'
#' @param id OSF id (osf.io/XXXX; just XXXX)
#' @param private Set project to private/public (default changes to public)
#'
#' @return Boolean, update success
#' @export
#'
#' @seealso \code{\link{update_project}}

update_component <- function(id, private = FALSE) {
  update_project(id, private)
}


#' Empty out a component and delete it
#'
#' This function removes all containing files of a component and then removes
#' the component itself. NOTE: it does not request confirmation, so please
#' handle with care. If you do not have backups of the files, it is easy to
#' lose everything (in the component).
#'
#' @param id OSF id (osf.io/xxxx; just XXXX)
#'
#' @return Boolean, deletion success
#' @export

delete_component <- function(id) {
  config <- get_config(TRUE)

  url_osf <- construct_link_files(id, request = '?confirm_delete=1')
  call <- httr::DELETE(url_osf, config)

  if (call$status_code != 204){
  	stop('Unable to delete node. Maybe it is not empty?
      You may want to enable recursive = TRUE')
  }

  url_osf <- construct_link(sprintf('nodes/%s', id))

  call <- httr::DELETE(url_osf, config)

  if (call$status_code != 204)
    stop('Unable to delete node. Maybe it is not empty?
  You may want to enable recursive = TRUE')

  return(TRUE)
}
