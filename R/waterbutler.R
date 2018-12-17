# Appends API version to a specificed path
# id: OSF project guid (e.g., fa9dm)
# fid: waterbutler file/folder id (e.g., 5beaf8e7a6a9af00166b4243)
# provider: storage provider (default: osfstorage)
wb_path <- function(id, fid = NULL, provider = "osfstorage") {
  if (is.null(fid)){
    sprintf("v%i/resources/%s/providers/%s/", floor(.wb_api_version), id, provider)
  } else {
    sprintf("v%i/resources/%s/providers/%s/%s/", floor(.wb_api_version), id, provider, fid)
  }
}


# Construct the WaterButler API Client
wb_cli <- function(pat = getOption("osfr.pat")) {

  url <- ifelse(Sys.getenv('OSF_USE_SERVER') == "test",
                   "https://files.us.test.osf.io",
                   "https://files.osf.io")

  headers <- list(
    `User-Agent` = user_agent()
  )

  if (!is.null(pat)) {
    headers$Authorization = sprintf('Bearer %s', pat)
  }

  crul::HttpClient$new(
    url = url,
    opts = list(
      timeout = 5,
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
# fid: waterbutler folder id (e.g., 5beaf8e7a6a9af00166b4243)
# v1/resources/fa9dm/providers/osfstorage/5beaf8e7a6a9af00166b4243/?kind=folder
.wb_create_folder <- function(id, name, fid) {
  query <- list(kind = "folder", name = name)
  res <- .wb_request("put", wb_path(id, fid), query = query)
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}

# url: new_folder link for the existing parent folder
# wb_create_subfolder <- function(id, name, parent_id) {
#   path <- file.path(wb_path(id), parent_id)
#   .wb_put(path, query = list(kind = "folder", name = name))
# }

# Upload a new file
# id: OSF node or waterbulder folder to upload
# name: desired name of the file
# body: raw file data
.wb_file_upload <- function(id, name, body) {
  query <- list(kind = "file", name = name)
  res <- .wb_request("put", wb_path(id), query = query, body = body)

  if (res$status_code == 409) {
    http_error(
      res$status_code,
      sprintf("File or folder already exists: %s", name)
    )
  }

  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}



