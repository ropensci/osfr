#' Download a file or folder from OSF
#'
#' Files stored on OSF can be downloaded locally by providing an
#' [`osf_tbl_file`] that contains the OSF file of interest. If the
#' [`osf_tbl_file`] contains a directory, a zip file containing the OSF
#' directory's contents will be downloaded.
#'
#' By default files are downloaded to your current working directory with the
#' same filename used on OSF. The `path` argument can be used to specify a
#' different destination and a different filename for the downloaded file.
#' However, the directory portion of `path` must reference an existing
#' directory.
#'
#' @param x an [`osf_tbl_file`] containing a single file or directory.
#' @param path Local path where the downloaded file will be saved. The default
#'   is to use the remote file's name.
#' @param overwrite Logical, if the local path already exists should it be
#'   replaced with the downloaded file?
#'
#' @return an [`osf_tbl_file`] with a new column, `"local_path"`, containing the
#'   downloaded file's path
#' @examples
#' \dontrun{
#' # Download a single file
#' analysis_plan <- osf_retrieve_file("2ryha")
#' analysis_plan <- osf_download(analysis_plan, path = "plan_wave1.docx")
#'
#' # Verify the file was downloaded locally
#' file.exists(analysis_plan$local_path)
#' }
#' @export
#' @importFrom fs path_ext_set

osf_download <- function(x, path = NULL, overwrite = FALSE) {
  UseMethod("osf_download")
}

#' @export
osf_download.osf_tbl_file <- function(x, path = NULL, overwrite = FALSE) {
  x <- make_single(x)
  if (is.null(path)) path <- x$name

  if (is_osf_dir(x)) {
    type <- "folder"
    path <- as.character(fs::path_ext_set(path, "zip"))
  } else {
    type <- "file"
  }

  if (!dir.exists(dirname(path))) {
    abort("The directory specified in `path` does not exist.")
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

  if ("local_path" %in% names(x)) {
    x$local_path <- path
  } else {
    x <- tibble::add_column(x, local_path = path, .before = "meta")
  }
  x
}
