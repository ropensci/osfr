# Appends API version to a specificed path
# id: OSF project guid
# provider: storage provider (default: osfstorage)
wb_path <- function(id, provider = "osfstorage") {
  sprintf("v%i/resources/%s/providers/%s/", floor(.wb_api_version), id, provider)
}

# Construct the WaterButler API Client
wb_cli <- function(pat = osf_pat()) {

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

.wb_get <- function(path, query = list(), ...) {
  cli <- wb_cli()
  res <- cli$get(path, query, ...)
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}

.wb_put <- function(path, query = list(), body = NULL, ...) {
  cli <- wb_cli()
  res <- cli$put(path, query, body, ...)
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
}


# Waterbutler API action endpoints ----------------------------------------
# https://waterbutler.readthedocs.io/en/latest/api.html#actions
wb_get_info <- function(id) {
  .wb_get(wb_path(id), query = list(meta = ""))
}

wb_create_subfolder <- function(id, name) {
  .wb_put(wb_path(id), query = list(kind = "folder", name = name))
}

