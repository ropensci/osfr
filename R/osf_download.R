#' Download files and folders from OSF
#'
#' @param path Local path for the downloaded file. The default is to use the
#'   remote file's name.

osf_download <- function(x, path = NULL, overwrite = FALSE) {
  NextMethod("osf_download")
}

osf_download.osf_tbl_file <- function(x, path = NULL, overwrite = FALSE) {
  x <- make_single(x)
  if (is_osf_dir(x)) {
    abort("Downloading an `osf_tbl_file` requires a file\n* `x` contains a directory")
  }

  if (is.null(path)) path <- x$name
  if (file.exists(path) && !overwrite) {
    abort("A file exists at the specified path and `overwrite` is `FALSE`")
  }

  out <- .wb_download(
    id = get_parent_id(x),
    fid = as_id(x),
    path = path,
    type = ifelse(is_osf_dir(x), "folder", "file"),
  )
  invisible(out)
}
