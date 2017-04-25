#' Upload a revised file to the OSF
#'
#' @param id OSF id (osf.io/XXXX) of file to revise
#' @param filename Local filename to upload as revision
#' @param \ldots Additional parameters passed to \code{\link{process_type}} and \code{\link{construct_link}}
#'
#' @return Boolean of revision success
#' @seealso \code{\link{upload_file}}, \code{\link{upload_new}}
upload_revision <- function(
  id = NULL,
  filename = NULL,
  ...) {

  if (is.null(filename))
    stop("Please input filename to be uploaded as revision.")

  # Assume it is private just in case
  # Incorporates login check needed anyway
  typ <- process_type(id, private = TRUE, ...)

  if (typ != "files")
    stop("Cannot upload revisions if not a file.")

  url_osf <- construct_link(sprintf("%s/%s", typ, id), ...)

  call <- httr::GET(
    url_osf,
    httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  res <- process_json(call)

  upload_osf <- res$data$links$upload

  upload <- httr::PUT(
    upload_osf,
    body = httr::upload_file(filename),
    encode = "raw",
    config = httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (upload$status_code != 200)
    stop("Failed to upload revision")

  res <- process_json(upload)

  return(res$data$links$download)
}
