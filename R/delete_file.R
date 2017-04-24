#' Delete a file based on OSF id
#'
#' @param id OSF id (osf.io/XXXX)
#' @param \ldots Additional parameters passed to \code{\link{process_file_id}}
#'
#' @return Boolean, delete succeeded?
#' @export
delete_file <- function(id = NULL, ...) {

  if (Sys.getenv("OSF_PAT") == "")
    stop("Requires login, use login()")
  if (is.null(id))
    stop("Specify node to delete")

  url_osf <- process_file_id(id, ...)

  call <- httr::DELETE(
    url = url_osf,
    httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (call$status_code != 204)
    stop("Failed to delete file. Is it surely a file?")

  return(TRUE)
}
