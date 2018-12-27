#' Upload files to OSF
#'
#' @param x an [`osf_tbl_node`] with a single project or
#'   component, or an [`osf_tbl_file`] with a single directory
#' @param path Path to file on local machine to upload. Ensure file has proper
#'   extension named (i.e., extension sensitive, not like on Linux)
#' @param name Name of the uploaded file (if `NULL`, `basename(path)`
#'   will be used).
#' @param overwrite Logical, overwrite an existing file with the same name
#'   (default `FALSE`)? If `TRUE`, OSF will automatically update the file and
#'   record the previous version.
#'
#' @return an [`osf_tbl_file`] containing uploaded file
#' @export
#' @importFrom crul upload
#' @importFrom fs is_file is_dir

osf_upload <- function(x, path, name = NULL, overwrite = FALSE) {
  if (!file.exists(path)) abort(sprintf("Can't find file:\n %s", path))
  if (is_dir(path)) abort("`path` must point to a file\n* Uploading directories is not supported")
  UseMethod("osf_upload")
}

#' @export
osf_upload.osf_tbl_node <- function(x, path, name = NULL, overwrite = FALSE) {
  if (is.null(name)) name <- basename(path)
  x <- make_single(x)

  # check if filename already exists at destination
  items <- osf_ls_files(x, type = "file", pattern = name)
  osf_file <- items[items$name == name, ]

  if (nrow(osf_file) == 0) {
    out <- upload_file(as_id(x), path, name)
  } else {
    out <- update_file(as_id(x), path, as_id(osf_file), overwrite)
  }

  as_osf_tbl(out["data"], "osf_tbl_file")
}

#' @export
osf_upload.osf_tbl_file <- function(x, path, name = NULL, overwrite = FALSE) {
  if (is.null(name)) name <- basename(path)
  x <- make_single(x)

  if (is_osf_file(x)) {
    abort("Uploading to an `osf_tbl_file` requires a directory\n* `x` contains a file")
  }

  items <- osf_ls_files(x, type = "file", pattern = name)
  osf_file <- items[items$name == name, ]

  if (nrow(osf_file) == 0) {
    out <- upload_file(get_parent_id(x), path, name, as_id(x))
  } else {
    out <- update_file(get_parent_id(x), path, as_id(osf_file), overwrite)
  }

  as_osf_tbl(out["data"], "osf_tbl_file")
}


upload_file <- function(id, path, name, dir_id = NULL) {
  res <- .wb_file_upload(id, name, crul::upload(path), dir_id)
  raise_error(res)

  # the metadata returned by waterbutler is a subset of what's returned by osf
  # so this extra API call allows us to return a consistent osf_tbl_file
  file_id <- strsplit(res$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}

update_file <- function(id, path, file_id, overwrite = TRUE) {
  if (!overwrite) {
    abort("File already exists at destination\n* Set `overwrite=TRUE` to upload a new version")
  }
  res <- .wb_file_update(id, file_id, body = crul::upload(path))
  raise_error(res)

  file_id <- strsplit(res$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}
