#' Delete a project from the OSF
#'
#' @param id OSF id (osf.io/xxxx)
#' @param recursive Boolean, if TRUE will go through folder nesting (see \code{maxdepth})
#' @param maxdepth Number of nesting levels to go through
#'
#' @return Boolean, delete succeeded?
#' @export

delete_project <- function(id = NULL,
                           recursive = FALSE,
                           maxdepth = 5,
                           ...)
{
  if (recursive == TRUE)
  {
    del.id <- recurse_node(id, public = FALSE, maxdepth, ...)

    for (each in del.id)
    {
      delete_empty(each, ...)

      cat(sprintf("Deleted subcomponent %s\n", each))
    }
  }
  else
  {
    delete_empty(id, ...)
  }
  return(TRUE)
}
