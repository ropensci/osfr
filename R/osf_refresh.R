#' Refresh an OSF entity
#'
#' Use `osf_refresh()` to update one or more entities in an [`osf_tbl()`] with
#' the latest information from OSF.
#'
#' @param x an [`osf_tbl`]

osf_refresh <- function(x) {
  stopifnot(inherits(x, "osf_tbl"))

  retrieve <- switch(class(x)[1],
                     osf_tbl_node = osf_retrieve_node,
                     osf_tbl_file = osf_retrieve_file,
                     osf_tbl_user = osf_retrieve_user
  )

  res <- lapply(as_id(x), retrieve)
  refreshed <- do.call("rbind", res)

  # update x to preserve any additional variables
  x[colnames(refreshed)] <- refreshed
  x
}
