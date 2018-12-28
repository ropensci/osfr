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

# empty data$list() is returned when resource doesn't exist
.wb_get_info <- function(id) {
  res <- .wb_request("get", wb_path(id), query = list(meta = ""))
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}

# id: OSF project/component GUID
# name: name of the new directory
# fid: waterbutler folder id (e.g., 5beaf8e7a6a9af00166b4243) if creating a
#   subfolder within an existing folder
# v1/resources/fa9dm/providers/osfstorage/5beaf8e7a6a9af00166b4243/?kind=folder
.wb_create_folder <- function(id, name, fid = NULL) {
  query <- list(kind = "folder", name = name)
  res <- .wb_request("put", wb_path(id, fid), query = query)
  process_response(res)
}

# url: new_folder link for the existing parent folder
# wb_create_subfolder <- function(id, name, parent_id) {
#   path <- file.path(wb_path(id), parent_id)
#   .wb_put(path, query = list(kind = "folder", name = name))
# }

# Upload a new file
# id: OSF node
# name: desired name of the file
# body: raw file data
# dir_id: optional, waterbutler ID for directory to upload to
.wb_file_upload <- function(id, name, body, dir_id = NULL) {
  query <- list(kind = "file", name = name)
  res <- .wb_request("put", wb_path(id, dir_id), query = query, body = body)
  process_response(res)
}

# Update an existing file
# file_id: waterbutler file id for existing file
.wb_file_update <- function(id, file_id, body) {
  query <- list(kind = "file")
  path <- wb_path(id, file_id, type = "file")
  res <- .wb_request("put", path, query = query, body = body)
  process_response(res)
}
