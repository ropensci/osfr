#' Create a component within a project
#'
#' @param id OSF id (osf.io/XXXX; just XXXX) of parent project
#' @param title Title of the component
#' @param description Description of the component
#' @param category Category of component, for valid categories
#'   see \code{\link{process_category}}
#' @param private Boolean of whether the component is supposed to be private
#'
#' @return OSF id of created component
#' @export
#' @seealso \code{\link{create_project}}

create_component <- function(
  id,
  title = '',
  description = '',
  category = '',
  private = TRUE) {

  if (is.null(id)) stop('Please input project id.')
  

  process_category(category)

  url_osf <- construct_link(sprintf('nodes/%s/children/', id))

  body <- list(
    data = list(
      type = 'nodes',
      attributes = list(
        title = title,
        category = category,
        description = description,
        public = !private
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

# update_component <- function() {
# }


#' Empty out a component and delete it
#'
#' @param id OSF id (osf.io/xxxx; just XXXX)
#'
#' @return Boolean, deletion succeeded?
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
