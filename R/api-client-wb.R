#' Generate Waterbutler API paths
#'
#' @param id GUID for an OSF project or component
#' @param fid waterbutler identifier for a file or folder
#' @param provider storage provider (default: osfstorage)
#' @param type indicate whether the provided `fid` refers to a `"folder"` (the
#'   default) or `"file"`. This is significant because the path must always have
#'   a trailing flash when referring to a folder
#' @param version Specify the waterbutler API version
#'
#' @noRd
.wb_api_path <-
  function(id,
           fid = NULL,
           provider = "osfstorage",
           type = "folder",
           version = 1) {

  type <- match.arg(type, c("folder", "file"))

  out <- sprintf("v%i/resources/%s/providers/%s/", version, id, provider)
  if (!is.null(fid)) out <- paste(out, fid, sep = "/")

  switch(type,
    file = sub("\\/$", "", out),
    folder = out
  )
}


# Waterbutler request functions -------------------------------------------

.wb_request <-
  function(method,
           path,
           query = list(),
           body = NULL,
           verbose = FALSE,
           ...) {

  method <- match.arg(method, c("get", "put", "patch", "post", "delete"))
  cli <- .build_client(api = "wb", encode = "raw")

  cli$retry(
    method,
    path,
    query,
    body = body,
    times = 3,
    retry_only_on = "502",
    onwait = retry_message,
    ...
  )
}
