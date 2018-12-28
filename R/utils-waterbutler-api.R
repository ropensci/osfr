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
wb_path <- function(id, fid = NULL, provider = "osfstorage", type = "folder") {
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
wb_cli <- function(pat = getOption("osfr.pat")) {

  url <- ifelse(Sys.getenv("OSF_USE_SERVER") == "test",
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
  method <- match.arg(method, c("get", "put", "patch", "delete"))
  cli <- wb_cli()
  method <- cli[[method]]
  method(path, query, body = body, ...)
}


# Waterbutler API action endpoints ----------------------------------------
# https://waterbutler.readthedocs.io/en/latest/api.html#actions


#' Create a folder or subfolder
#'
#' @param id GUID for an OSF project or component
#' @param name Name of the new directory
#' @param fid Optional, provide a Waterbutler folder ID to create the new folder
#'   within the specified existing folder
#'
#' @noRd
.wb_create_folder <- function(id, name, fid = NULL) {
  query <- list(kind = "folder", name = name)
  res <- .wb_request("put", wb_path(id, fid), query = query)
  process_response(res)
}

#' Upload a new file
#'
#' @inheritParams .wb_create_folder
#' @param name Name of the uploaded file
#' @param body Raw file data
#' @param fid: Optional, Waterbutler folder ID to upload the file directly to
#'   the specified existing folder
#'
#' @noRd
.wb_file_upload <- function(id, name, body, fid = NULL) {
  query <- list(kind = "file", name = name)
  res <- .wb_request("put", wb_path(id, fid), query = query, body = body)
  process_response(res)
}

#' Update an existing file
#'
#' @inheritParams .wb_create_folder
#' @inheritParams .wb_file_upload
#' @param fid Existing file's Waterbutler ID
#'
#' @noRd
.wb_file_update <- function(id, fid, body) {
  query <- list(kind = "file")
  path <- wb_path(id, fid, type = "file")
  res <- .wb_request("put", path, query = query, body = body)
  process_response(res)
}

#' Download a file
#'
#' @inheritParams .wb_create_folder
#' @param fid Waterbutler ID for the file or folder to download
#' @param path local path where the downloaded file will be saved
#' @param type indicate whether downloading a `"file"` or `"folder"`
#' @param zip Logical, should the downloaded contents be zipped? Only applies to
#'   folders.
#'
#' @noRd
.wb_download <- function(id, fid, path, type, zip = FALSE) {
  type <- match.arg(type, c("file", "folder"))
  res <- .wb_request("get", wb_path(id, fid, type = type), disk = path)
  if (res$status_code == 200) return(TRUE)
  if (res$status_code == 404) {
    msg <- sprintf("The requested %s (%s) could not be found in node `%s`",
                   type, fid, id)
    abort(msg)
  }
  res$raise_for_status()
}
