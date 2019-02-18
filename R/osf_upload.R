#' Upload files to OSF
#'
#' @param x an [`osf_tbl_node`] with a single project or
#'   component, or an [`osf_tbl_file`] with a single directory
#' @param path Path to a local file. Ensure the file has a proper
#'   file extension (e.g., `.docx`) to ensure it's rendered properly on OSF.
#' @param name Name of the uploaded file (if `NULL`, `basename(path)`
#'   will be used).
#' @param overwrite Logical, overwrite an existing file with the same name
#'   (default `FALSE`)? If `TRUE`, OSF will automatically update the file and
#'   record the previous version.
#' @template verbose
#'
#' @return an [`osf_tbl_file`] containing uploaded file
#'
#' @examples
#' \dontrun{
#' # Create an example file to upload to our example project
#' write.csv(iris, file = "iris.csv")
#' project <- osf_create_project("Flower Data")
#'
#' # Upload the first version
#' osf_upload(project,"iris.csv")
#'
#' # Modify the data file, upload version 2, and view it on OSF
#' write.csv(subset(iris, Species != "setosa"), file = "iris.csv")
#' project %>%
#'   osf_upload("iris.csv", overwrite = TRUE) %>%
#'   osf_open()
#' }
#'
#' @seealso
#' * [`osf_download()`] for downloading files and directories from OSF.
#' * [`osf_ls_files()`] for listing files and directories on OSF.
#'
#' @export
#' @importFrom crul upload
#' @importFrom fs is_file is_dir path_dir

osf_upload <-
  function(x,
           path,
           name = NULL,
           overwrite = FALSE,
           verbose = FALSE) {

  if (!file.exists(path)) abort(sprintf("Can't find file:\n %s", path))
  if (is_dir(path)) {
    abort("`path` must point to a file\n* Uploading directories is not supported")
  }
  UseMethod("osf_upload")
}

#' @export
osf_upload.osf_tbl_node <-
  function(x,
           path,
           name = NULL,
           overwrite = FALSE,
           verbose = FALSE) {

  if (is.null(name)) name <- basename(path)
  name <- check_upload_name(name)
  x <- make_single(x)

  # check if filename already exists at destination
  items <- osf_ls_files(x, type = "file", pattern = name)
  osf_file <- items[items$name == name, ]

  if (nrow(osf_file) == 0) {
    out <- upload_file(as_id(x), path, name, verbose = verbose)
  } else {
    out <- update_file(as_id(x), path, as_id(osf_file), overwrite, verbose)
  }

  as_osf_tbl(out["data"], "osf_tbl_file")
}

#' @export
osf_upload.osf_tbl_file <-
  function(x,
           path,
           name = NULL,
           overwrite = FALSE,
           verbose = FALSE) {

  if (is.null(name)) name <- basename(path)
  name <- check_upload_name(name)
  x <- make_single(x)

  if (is_osf_file(x)) {
    abort("Uploading to an `osf_tbl_file` requires a directory\n* `x` contains a file")
  }

  items <- osf_ls_files(x, type = "file", pattern = name)
  osf_file <- items[items$name == name, ]

  if (nrow(osf_file) == 0) {
    out <- upload_file(get_parent_id(x), path, name, as_id(x), verbose)
  } else {
    out <- update_file(get_parent_id(x), path, as_id(osf_file), overwrite, verbose)
  }

  as_osf_tbl(out["data"], "osf_tbl_file")
}

check_upload_name <- function(x) {
  path <- fs::path_dir(x)
  if (path != ".") {
    x <- basename(x)
    warn(sprintf("Removing path information (%s) from uploaded file name", path))
  }
  x
}

upload_file <- function(id, path, name, dir_id = NULL, verbose = FALSE) {
  res <- .wb_file_upload(id, name, crul::upload(path), dir_id)
  raise_error(res)

  if (verbose) message(sprintf("Uploaded %s to OSF", basename(path)))

  # the metadata returned by waterbutler is a subset of what's returned by osf
  # so this extra API call allows us to return a consistent osf_tbl_file
  file_id <- strsplit(res$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}

update_file <- function(id, path, file_id, overwrite = TRUE, verbose = FALSE) {
  if (!overwrite) {
    abort("File already exists at destination\n* Set `overwrite=TRUE` to upload a new version")
  }

  res <- .wb_file_update(id, file_id, body = crul::upload(path))
  raise_error(res)

  if (verbose) message(sprintf("Uploaded new version of %s to OSF", basename(path)))

  file_id <- strsplit(res$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}
