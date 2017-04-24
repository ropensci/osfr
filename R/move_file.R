#' Move (and copy) files on the OSF
#'
#' @param from OSF file id to move (osf.io/xxxx)
#' @param to OSF id to move to (osf.io/xxxx; needs to be component)
#' @param filename Optional, rename the file
#' @param action Move or copy
#' @param conflict Keep old file or replace in case of conflict
#' @param \ldots Additional parameters passed to \code{\link{process_type}}
#'
#' @return Boolean, moving succeeded?
#' @export
move_file <- function(
  from = NULL,
  to = NULL,
  filename = NULL,
  action = "move",
  conflict = "replace",
  ...) {

  if (nchar(from) == 5) {
    typfrom <- process_type(id = from, ...)
    typto <- process_type(id = to, ...)

    if (typfrom != "nodes" & typto != "nodes")
      stop("Needs to move from node to node")

    url_osf <- process_file_id(from, ...)
  } else {
    typto <- process_type(id = to, ...)

    if (typto != "nodes")
      stop("Needs to move from node to node")

    url_osf <- from
  }

  body <- list(
    action = action,
    path = "/",
    rename = filename,
    conflict = conflict,
    provider = "osfstorage",
    resource = to)

  call <- httr::POST(
    url_osf,
    body = body, encode = "json",
    httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (call$status_code != 201 & call$status_code != 200)
    stop("Error in moving/copying file.")

  return(TRUE)
}
