#' Build an API Client
#'
#' Create a crul HTTP client for either osf or waterbutler.
#'
#' @param api Either `"osf"` or `"wb"` for waterbutler
#' @param version Optional, specify the API version string (e.g., `"2.20"`).
#'   Only used when `api = "osf"`. When provided, the version is embedded in
#'   the `Accept` header to pin requests to a minor version of the API.
#'   When `NULL` (the default), the `Accept` header requests JSON:API content
#'   without pinning to a specific version.
#' @param encode one of `"form"`, `"multipart"`, `"json"`, or `"raw"`.
#' @noRd
.build_client <-
  function(api,
           encode,
           version = NULL,
           progress = NULL,
           pat = getOption("osfr.pat")) {

  api <- match.arg(api, c("osf", "wb"))
  encode <- match.arg(encode, c("form", "multipart", "json", "raw"))
  server <- Sys.getenv("OSF_SERVER")

  url <- switch(api,
    osf = ifelse(nzchar(server), "api.%s.osf.io",      "api.osf.io"),
    wb  = ifelse(nzchar(server), "files.us.%s.osf.io", "files.osf.io")
  )

  if (nzchar(server)) url <- sprintf(url, server)

  # assemble headers
  headers <- list(`User-Agent` = user_agent())

  if (!is.null(pat)) {
    headers$Authorization <- sprintf("Bearer %s", pat) # nolint
  }

  if (api == "osf") {
    headers$Accept <- if (!is.null(version)) { # nolint
      sprintf("application/vnd.api+json;version=%s", version)
    } else {
      "application/vnd.api+json"
    }
  }

  crul::HttpClient$new(
    url = paste0("https://", url),
    opts = list(
      encode = encode
    ),
    headers = headers,
    hooks = list(
      request = log_request,
      response = log_response
    ),
    progress = progress
  )
}
