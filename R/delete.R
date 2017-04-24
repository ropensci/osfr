#' Delete a project or file from the OSF
#'
#' @param id OSF id (osf.io/xxxx)
#' @param recursive Boolean, if TRUE will go through folder nesting (see \code{maxdepth})
#' @param maxdepth Number of nesting levels to go through
#' @param \ldots Additional parameters passed to \code{\link{process_type}}, \code{\link{delete_project}}, and \code{\link{delete_file}}
#'
#' @return Boolean, deletion succeeded?
#' @export
delete <- function(
  id = NULL,
  recursive = FALSE,
  maxdepth = 5,
  ...) {

  if (is.null(id))
    stop("I require an OSF id.")

  typ <- process_type(id, ...)

  if (typ == "nodes")
    delete_project(id, recursive, maxdepth, ...)

  if (typ == "files")
    delete_file(id, ...)
}
