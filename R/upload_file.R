#' Upload a file to the OSF (both new and revised)
#'
#' @param id OSF id (osf.io/XXXX) to upload to. Specify project to upload new file,
#'  specify a file to upload a revision
#' @param filename Local filename to upload
#' @param \ldots Additional parameters passed to \code{\link{process_type}}, \code{\link{upload_new}}, and \code{\link{upload_revision}}
#'
#' @return Boolean of upload success
#' @export
#'
#' @examples
#' \dontrun{
#' upload_file(id = '12345', filename = 'test.pdf')
#' }
upload_file <- function(
  id = NULL,
  filename = NULL,
  ...) {

  if (is.null(id))
    stop("Input component to upload to.")

  if (process_type(id, ...) == "nodes") {
    upload_new(id, filename, ...)
  } else if (process_type(id, ...) == "files") {
    upload_revision(id, filename, ...)
  } else {
    stop("Something odd happened. Eat chocolate and life will be better.\n
         If the problem persists, consider issuing a bug report on
         github.com/chartgerink/osfr")
  }
}
