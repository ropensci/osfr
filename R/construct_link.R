#' Construct an API link with proper base
#'
#' @param request The request link to be combined with the base API link.
#' @param test Whether the link should be constructed for testing server
#'
#' @return The full request link with proper base
#' @examples
#' \dontrun{
#' construct_link("nodes/{node_id}/files/")
#' }
construct_link <- function(request = NULL, test = FALSE) {
  if (test == FALSE)
    base <- "https://api.osf.io/v2/"

  if (test == TRUE)
    base <- "https://test-api.osf.io/v2/"

  result <- paste0(base, request)

  return(result)
}

#' Construct a waterbutler API link with proper base
#'
#' @param id OSF id
#' @param request Request for waterbutler
#' @param test Whether the link should be constructed for testing server
#'
#' @return Waterbutler link
construct_link_files <- function(id = NULL, request = NULL, test = FALSE) {
  if (test == FALSE) {
    base <- sprintf("https://files.osf.io/v1/resources/%s/providers/osfstorage/%s",
      id, request)
  }

  if (test == TRUE) {
    base <- sprintf("https://test-files.osf.io/v1/resources/%s/providers/osfstorage/%s",
      id, request)
  }

  return(base)
}
