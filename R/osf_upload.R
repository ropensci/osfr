#' Upload files to OSF
#'
#' @param x an \code{osf_tbl_node} with a single project or
#'   component, \code{osf_tbl_file} with a single directory
#' @param path Path to file on local machine to upload. Ensure file has proper
#'   extension named (i.e., extension sensitive, not like on Linux)
#' @param name Name of the uploaded file (if `NULL`, `basename(path)`
#'   will be used).
#' @param overwrite Logical, overwrite an existing file with the same name
#'   (default `FALSE`)? If `TRUE`, OSF will automatically update the file and
#'   record the previous version.
#'
#' @return \code{osf_tbl_file} containing uploaded file
#' @export
#' @importFrom crul upload
#' @importFrom fs is_file

osf_upload <- function(x, path, name = NULL, overwrite = FALSE) {
  if (!file.exists(path)) abort(sprintf("Can't find file:\n %s", path))
  if (!is_file(path)) abort("`path` must point to a file")
  UseMethod("osf_upload")
}

#' @export
osf_upload.osf_tbl_node <- function(x, path, name = NULL, overwrite = FALSE) {
  x <- make_single(x)
  id <- as_id(x)
  if (is.null(name)) name <- basename(path)

  out <- .wb_file_upload(id, name, body = crul::upload(path))

  # file already exists at destination
  if (!is.null(out$status_code)) {
    if (out$status_code == 409 && overwrite) {
      items <- osf_ls_files(x, type = "file", pattern = name)

      # check for an exact match because the OSF filter is based on substring
      # matching, which can return multiple hits
      file <- items[items$name == name, ]
      out <- .wb_file_update(id, file$id[1], body = crul::upload(path))
    }
  }
  raise_error(out)

  # the metadata returned by waterbutler is a subset of what's returned by osf
  # so this extra API call allows us to return a consistent osf_tbl_file
  file_id <- strsplit(out$data$id, split = "/", fixed = TRUE)[[1]][2]
  out <- .osf_file_retrieve(file_id)

  as_osf_tbl(out["data"], "osf_tbl_file")
}

