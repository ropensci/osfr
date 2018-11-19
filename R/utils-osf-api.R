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



# OSF API request functions -----------------------------------------------

.osf_request <- function(method, path, query = list(), body = NULL, verbose = FALSE, ...) {
  method <- match.arg(method, c("get", "put", "patch", "delete"))
  cli <- osf_cli()
  method <- cli[[method]]
  method(path, query, body = body, ...)
}

.osf_paginated_request <- function(method, path, query = list(), n_max = Inf, verbose = FALSE) {
  items <- list()
  i <- 1
  total <- 0

  repeat {
    res <- .osf_request(method, path, query = list(page = i))
    out <- jsonlite::fromJSON(res$parse("UTF-8"), simplifyVector = FALSE)
    total <- total + length(out$data)
    items <- c(items, out$data)
    if (verbose && i == 2) message("Items retrieved so far:")
    if (verbose && i > 1) message(total, appendLF = TRUE)
    if (is.null(out$links$`next`) || total >= n_max) {
      if (verbose && i > 1) message("")
      break
    }
    i <- i + 1
  }
  items
}



# OSF API endpoints -------------------------------------------------------

# e.g., .osf_node_retrieve("k35ut)
.osf_node_retrieve <- function(id) {
  res <- .osf_request("get", osf_path(sprintf("nodes/%s/", id)))
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}

# e.g., .osf_node_delete("k35ut)
.osf_node_delete <- function(id) {
  res <- .osf_request("delete", osf_path(sprintf("nodes/%s/", id)))

  # since this endpoint doesn't return any useful info we'll return TRUE if
  # successful or the error message if not
  if (res$status_code == 204) return(TRUE)
  out <- jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
  http_error(res$status_code, out$errors[[1]]$detail)
}

# Retrieves the details for a given user
.osf_user_retrieve <- function(id = "me") {
  path <- osf_path(sprintf("/users/%s/", id))
  res <- .osf_request("get", path)
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}

# e.g., .osf_file_retrieve("5be5e1fdfe3eca00188178c3")
.osf_file_retrieve <- function(id ) {
  res <- .osf_request("get", osf_path(sprintf("files/%s/", id)))
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}
