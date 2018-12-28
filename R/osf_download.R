#' Download an OSF files or folder
#'
#' Files stored in OSF projects, components, or folders can be downloaded
#' locally by providing the appropriate [`osf_tbl_file`] to `osf_download()`.
#' Directories are downloaded as zip files.
#'
#' @param an [`osf_tbl_file`] containing a file or directory
#' @param path Local path where the downloaded file will be saved. The default
#'   is to use the remote file's name.
#' @param overwrite Logical, if the local path already exists should it be
#'   replaced with the downloaded file?
#'
#' @importFrom fs path_ext_set

osf_download <- function(x, path = NULL, overwrite = FALSE) {
  NextMethod("osf_download")
}

osf_download.osf_tbl_file <- function(x, path = NULL, overwrite = FALSE) {
  x <- make_single(x)
  if (is.null(path)) path <- x$name

  if (is_osf_dir(x)) {
    type <- "folder"
    path <- fs::path_ext_set(path, "zip")
  } else {
    type <- "file"
  }

  if (file.exists(path) && !overwrite) {
    abort("A file exists at the specified path and `overwrite` is `FALSE`")
  }

  out <- .wb_download(
    id = get_parent_id(x),
    fid = as_id(x),
    path = path,
    type = type,
    zip = type == "folder"
  )
  invisible(out)
}
