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
  res <- .wb_request("put", .wb_api_path(id, fid), query = query)
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

.wb_file_upload <- function(id, name, body, fid = NULL, progress = FALSE) {
  query <- list(kind = "file", name = name)
  path <- .wb_api_path(id, fid)
  res <- .wb_request("put", path, query = query, body = body, progress = progress)
  process_response(res)
}

#' Update an existing file
#'
#' @inheritParams .wb_create_folder
#' @inheritParams .wb_file_upload
#' @param fid Existing file's Waterbutler ID
#'
#' @noRd
.wb_file_update <- function(id, fid, body, progress = FALSE) {
  query <- list(kind = "file")
  path <- .wb_api_path(id, fid, type = "file")
  res <- .wb_request("put", path, query = query, body = body, progress = progress)
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
#' @return Invisibly returns `TRUE` when the download succeeds
#'
#' @noRd
.wb_download <-
  function(id,
           fid,
           path,
           type,
           zip = FALSE,
           verbose = FALSE,
           progress = FALSE) {
  type <- match.arg(type, c("file", "folder"))
  query <- list()
  if (zip) query$zip <- ""
  api_path <- .wb_api_path(id, fid, type = type)

  if (progress) cat(sprintf("Downloading %s\n", basename(path)))
  res <- .wb_request("get", api_path, query, disk = path, progress = progress)
  if (verbose) message(sprintf("Downloaded %s to %s", type, path))

  if (res$status_code == 200) return(invisible(TRUE))
  if (res$status_code == 404) {
    msg <- sprintf("The requested %s (%s) could not be found in node `%s`",
                   type, fid, id)
    abort(msg)
  }
  res$raise_for_status()
}
