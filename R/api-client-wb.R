#' Generate Waterbutler API paths
#'
#' @param id GUID for an OSF project or component
#' @param fid waterbutler identifier for a file or folder
#' @param provider storage provider (default: osfstorage)
#' @param type indicate whether the provided `fid` refers to a `"folder"` (the
#'   default) or `"file"`. This is significant because the path must always have
#'   a trailing flash when referring to a folder
#'
#' @noRd
.wb_api_path <- function(id, fid = NULL, provider = "osfstorage", type = "folder") {
  type <- match.arg(type, c("folder", "file"))
  api_v <- floor(.wb_api_version)
  if (is.null(fid)) {
    out <- sprintf("v%i/resources/%s/providers/%s/", api_v, id, provider)
  } else {
    out <- sprintf("v%i/resources/%s/providers/%s/%s/", api_v, id, provider, fid)
  }
  switch(type,
    file = sub("\\/$", "", out),
    folder = out
  )
}


# Construct the WaterButler API Client
.wb_cli <- function(pat = getOption("osfr.pat")) {

  url <- ifelse(Sys.getenv("OSF_SERVER") == "test",
                   "https://files.us.test.osf.io",
                   "https://files.osf.io")

  headers <- list(
    `User-Agent` = user_agent()
  )

  if (!is.null(pat)) {
    headers$Authorization <- sprintf("Bearer %s", pat)
  }

  crul::HttpClient$new(
    url = url,
    opts = list(
      encode = "raw"
    ),
    headers = headers
  )
}



# Waterbutler request functions -------------------------------------------

.wb_request <- function(method, path, query = list(), body = NULL, verbose = FALSE, ...) {
  method <- match.arg(method, c("get", "put", "patch", "post", "delete"))
  cli <- .wb_cli()
  method <- cli[[method]]
  method(path, query, body = body, ...)
}
