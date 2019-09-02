#' Refresh an OSF entity
#'
#' Use `osf_refresh()` to update one or more entities in an [`osf_tbl()`] with
#' the latest information from OSF.
#'
#' @param x an [`osf_tbl`].
#' @export

osf_refresh <- function(x) {
  stopifnot(inherits(x, "osf_tbl"))

  retrieve <- switch(class(x)[1],
                     osf_tbl_node = osf_retrieve_node,
                     osf_tbl_file = osf_retrieve_file,
                     osf_tbl_user = osf_retrieve_user
  )

  refreshed <- map_rbind(retrieve, as_id(x))

  # update x to preserve any additional variables
  x[colnames(refreshed)] <- refreshed
  x
}
