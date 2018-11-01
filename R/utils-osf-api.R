#' Construct an API link with proper base
#'
#' Building urls for requests. OSF_USE_SERVER can be specified in the
#' environment to use a test or staging server.
#'
#' @param request The request link to be combined with the base API link.
#'
#' @return The full request link with proper base.
#'
#' @examples
#' \dontrun{
#' construct_link("nodes/")}

construct_link <- function(request) {
  base <- 'https://api.osf.io/v2/'

  if (!Sys.getenv('OSF_USE_SERVER') == '') {
    base <- sprintf('https://api.%s.osf.io/v2/', Sys.getenv('OSF_USE_SERVER'))
  }

  return(paste0(base, request))
}


#' Retrieve OSF personal access token.
#'
#' A OSF personal access token
#' Looks in env var \code{OSF_PAT}
#'
#' @keywords internal
#' @noRd
osf_pat <- function(quiet = TRUE) {
  pat <- Sys.getenv('OSF_PAT')
  if (!nzchar(pat)) return(NULL)

  if (!quiet) {
    message("Using OSF PAT from envvar OSF_PAT")
  }
  pat
}

# Return a version specific user agent
user_agent <- function(agent = "osfr") {
  version <- system.file("DESCRIPTION", package = "osfr", mustWork = FALSE)
  if (file.exists(version)) {
    version <- base::read.dcf(version, "Version")
    sprintf("%s v%s", agent, version)
  } else {
    agent
  }
}


# Appends API version to a specificed path
osf_path <- function(path) {
  sprintf("v%s/%s", floor(.osf_api_version), path)
}

# Construct the OSF API Client
osf_cli <- function(pat = osf_pat()) {
  server <- Sys.getenv('OSF_USE_SERVER')
  url <- if (nzchar(server)) {
    sprintf("https://api.%s.osf.io", server)
  } else {
    "https://api.osf.io"
  }

  headers <- list(
    `User-Agent` = user_agent(),
    `Accept-Header` = sprintf("application/vnd.api+json;version=%s", .osf_api_version)
  )

  if (!is.null(pat)) {
    headers$Authorization = sprintf('Bearer %s', pat)
  }

  crul::HttpClient$new(
    url = url,
    opts = list(
      timeout = 5,
      encode = "json"
    ),
    headers = headers
  )
}
