#' Upload a new file to the OSF.
#'
#' @param id Parent OSF id (osf.io/XXXX) to upload to.
#' @param filename Filename on local machine to upload
#' @param \ldots Additional parameters passed to \code{\link{process_type}} and \code{\link{construct_link_files}}
#'
#' @return Waterbutler URL
#' @seealso \code{\link{upload_file}}, \code{\link{upload_revision}}
upload_new <- function(
  id = NULL,
  filename = NULL,
  ...) {

  if (is.null(filename))
    stop("Please input filename to be uploaded.")

  # Assume it is private just in case
  # Incorporates login check needed anyway
  typ <- process_type(id, private = TRUE, ...)

  if (typ != "nodes")
    stop("Cannot upload new file if no node ID is specified.")

  url_osf <- construct_link_files(id,
    request = sprintf("?kind=file&name=%s", basename(filename)), ...)

  url_osf <- gsub(url_osf, pattern = "\\s", replacement = "%20", perl = TRUE)

  call <- httr::PUT(
    url_osf,
    body = httr::upload_file(filename),
    encode = "raw",
    config = httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (call$status_code == 409)
    stop("Conflict in filename naming. Please use upload_revision or change filename")
  if (call$status_code != 201)
    stop("Unsuccessful upload.")

  res <- process_json(call)

  return(res$data$links$download)
}
