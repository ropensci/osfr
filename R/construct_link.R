#' Construct an API link with proper base

#' @param request The request link to be combined with the base API link.
#' @return The full request link with proper base
#' @examples
#' construct.link("nodes/NODE_ID/files/")

construct.link <- function(request = NULL){
  base <- "https://staging2-api.osf.io/v2/"

  result <- paste0(base, request)

  return(result)
}
