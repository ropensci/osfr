#' Function to empty out a component and delete it
#'
#' @param id OSF id (osf.io/xxxx)
#'
#' @return Boolean, deletion succeeded?
#' @export
delete_empty <- function(id, ...) {

  if (is.null(id))
    stop("I require an OSF id.")

  url.osf <- construct_link_files(id, request = "?confirm_delete=1", ...)
  call <- httr::DELETE(url.osf, httr::add_headers(Authorization = sprintf(
    "Bearer %s",
    login())))

  if (call$status_code != 204)
    stop("Unable to delete node. Maybe it's not empty?\n
      You may want to enable recursive = TRUE")

  url.osf <- construct_link(sprintf("nodes/%s", id), ...)

  call <- httr::DELETE(url.osf,
    httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (call$status_code != 204)
    stop("Unable to delete node. Maybe it's not empty?\n
      You may want to enable recursive = TRUE")

  return(TRUE)
}
