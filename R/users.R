#' Retrieve OSF user information
#' @param id A valid OSF GUID
#'
#' @export
osf_user_retrieve <- function(id = "me") {
  out <- .osf_user_retrieve(id)
  raise_error(out)
  as_osf_tbl(out['data'], "osf_tbl_user")
}
