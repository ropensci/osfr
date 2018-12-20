#' Retrieve OSF user information
#'
#' @export
osf_user_retrieve <- function(id = "me") {
  out <- .osf_user_retrieve(id)
  raise_error(out)
  as_osf_tbl_user(out['data'])
}
