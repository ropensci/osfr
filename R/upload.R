#' Upload to OSF
#'
#' @param x something that identifies a valid OSF upload destination. This can
#'   be one of the following: an \code{osf_tbl_node} with a single project or
#'   component, an \code{osf_tbl_file} with a single directory, an OSF ID
#'   referencing a project, component, or directory
#' @param path Path to file on local machine to upload. Ensure file has proper
#'   extension named (i.e., extension sensitive, not like on Linux)
#' @param name Name of the uploaded file (if \code{NULL}, \code{basename(path)}
#'   will be used).
#'
#' @return Waterbutler URL
#' @export
#' @seealso \code{\link{upload_files}}, \code{\link{upload_revised_files}}
#' @importFrom crul upload
#' @importFrom fs is_file

osf_upload <- function(x, path, name = NULL, overwrite = FALSE) {
  if (!file.exists(path)) abort(sprintf("Can't find file:\n %s", path))
  if (!is_file(path)) abort("`path` must be a file")
  UseMethod("osf_upload")
}

#' @export
osf_upload.character <- function(x, path, name = NULL, overwrite = FALSE) {
  id <- as_id(x)
  if (!id_type(id) %in% c("files", "nodes")) {
    abort(sprintf("%s does not identify a valid upload destination."))
  }

  osf_upload(id, path, name, overwrite)
}

#' @export
osf_upload.osf_tbl <- function(x, path, name = NULL, overwrite = FALSE) {
  if (inherits(x, "osf_tbl_user")) {
    abort("An `osf_tbl_user` is not a valid upload destination.")
  }

  osf_upload(as_id(x), path, name, overwrite)
}

#' @export
osf_upload.osf_id <- function(x, path, name = NULL, overwrite = FALSE) {
  if (is.null(name)) name <- basename(path)
  out <- .wb_file_upload(x, name, body = crul::upload(path))

  # file already exists at destination
  if (!is.null(out$status_code)) {
    if (out$status_code == 409 && overwrite) {
      file <- osf_ls(x, type = "file", pattern = name)
      # check for an exact match because the OSF filter is based on substring
      # matching, which can return multiple hits
      file <- file[file$name == name, ]
      out <- .wb_file_update(x, file$id[1], body = crul::upload(path))
    }
  }
  raise_error(out)

  # the metadata returned by waterbutler is a subset of what's returned by osf
  # so this extra API call allows us to return a consistent osf_tbl_file
  file_id <- strsplit(out$data$id, split = "/", fixed = TRUE)[[1]][2]
  out <- .osf_file_retrieve(file_id)

  as_osf_tbl_file(out['data'])
}



