#' Construct an API link with proper base
#'
#' Building urls for requests. OSF_USE_SERVER can be specified in the
#' environment to use a test or staging server.
#'
#' @param request The request link to be combined with the base API link.
#'
#' @return The full request link with proper base.

construct_link <- function(request) {
  base <- 'https://api.osf.io/v2/'
  if (!Sys.getenv('OSF_USE_SERVER') == '') {
    base <- sprintf('https://%s-api.osf.io/v2/', Sys.getenv('OSF_USE_SERVER'))
  }

  return(paste0(base, request))
}
