#' Upload files to OSF
#'
#' Upload local files to a project, component, or directory on OSF.
#'
#' @param x The upload destintation on OSF. Can be one of the following:
#'   * An [`osf_tbl_node`] with a single project or component.
#'   * An [`osf_tbl_file`] with a single directory.
#' @param path A character vector of paths pointing to existing
#'   local files and/directories.
#' @param recurse If `TRUE`, fully recurse directories included in `path`. You
#'   can also control the number of levels to recurse by specifying a positive
#'   number.
#' @template conflicts
#' @template progress
#' @template verbose
#'
#' @return An [`osf_tbl_file`] containing the uploaded files and directories
#'   that were directly specified in `path`.
#'
#' @section File and directory paths:
#' The `x` argument indicates *where* on OSF the files will be uploaded (*i.e.*,
#' the destination). The `path` argument indicates *what* will be uploaded,
#' which can include a combination of files *and* directories.
#'
#' When `path` points to a local file, the file is uploaded to the *root* of the
#' specified OSF destination, regardless of where it's on your local machine
#' (*i.e.*, the intermediate paths are not preserved). For example, the
#' following would would upload both `a.txt` and `b.txt` to the root of
#' `my_proj`:
#'
#' ```
#' osf_upload(my_proj, c("a.txt", "subdir/b.txt"))`
#' ```
#'
#' When `path` points to a local directory, a corresponding directory will be
#' created at the root of the OSF destination, `x`, and any files within the
#' local directory are uploaded to the new OSF directory. Therefore, we could
#' maintain the directory structure in the above example by passing `b.txt`'s
#' parent directory to `path` instead of the file itself:
#'
#' ```
#' osf_upload(my_proj, c("a.txt", "subdir2"))
#' ```
#'
#' These behaviors are intended to provide flexibility
#'  make it convenient to replicate your
#' project directory on OSF and subsequently update the files as needed. In that
#' vein, `osf_upload(my_proj, path = ".")` will upload your entire current
#' working directory to the specified OSF destination.
#'
#' @section Uploading to subdirectories:
#' In order to upload directly to an existing OSF directory you would first need
#' to retrieve the directory as an [`osf_tbl_file`]. This can be accomplished by
#' passing the directory's unique identifier to [`osf_retrieve_file()`], or, if
#' you don't have the ID handy, you can use [`osf_ls_files()`] to retrieve the
#' directory by name.
#'
#' ```
#' # search for the 'rawdata' subdirectory within top-level 'data' directory
#' target_dir <- osf_ls_files(my_proj, path = "data", pattern = "rawdata")
#' # upload 'a.txt' to data/rawdata/ on OSF
#' osf_upload(target_dir, path = "a.txt")
#' ```
#'
#' @template synchronization
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
#' @importFrom memoise memoise forget

osf_upload <-
  function(x,
           path,
           recurse = FALSE,
           conflicts = "error",
           progress = FALSE,
           verbose = FALSE) {
  UseMethod("osf_upload")
}

#' @export
osf_upload.osf_tbl_node <-
  function(x,
           path,
           recurse = FALSE,
           conflicts = "error",
           progress = FALSE,
           verbose = FALSE) {

  path <- check_files(path)
  x <- make_single(x)
  .osf_upload(x, path, recurse, conflicts, progress, verbose)
}

#' @export
osf_upload.osf_tbl_file <-
  function(x,
           path,
           recurse = FALSE,
           conflicts = "error",
           progress = FALSE,
           verbose = FALSE) {

  path <- check_files(path)
  x <- make_single(x)

  if (is_osf_file(x)) {
    abort(paste0(
      "Can't upload directly to a file.\n",
      "Are you trying to update an existing file on OSF? Try:\n",
      "  * uploading to the file's parent directory or project/component"
    ))
  }

  .osf_upload(x, path, recurse, conflicts, progress, verbose)
}


#' Recursive file and directory upload function
#'
#' This function maps over all elements in `path` and recursively walks any
#' subdirectories, calling `file_upload()` for each file it counters..
#' @param dest OSF node or directory upload destination
#' @importFrom fs is_dir file_info
#' @noRd

.osf_upload <- function(dest, path, recurse, conflicts, progress, verbose) {

  # split into top-level files and folders
  path_by <- split(path, fs::file_info(path)$type, drop = TRUE)
  results <- vector(mode = "list")

  # upload files in pwd
  if (!is.null(path_by$file)) {
    results$file <- purrr::map(path_by$file,
      .upload_file,
      dest = dest,
      conflicts = conflicts,
      progress = progress,
      verbose = verbose
    )
  }

  # recurse directories in pwd
  if (!is.null(path_by$directory)) {
    results$directory <- purrr::map(path_by$directory,
      .upload_dir,
      dest = dest,
      conflicts = conflicts,
      recurse = recurse,
      progress = progress,
      verbose = verbose
    )
  }

  do.call("rbind", c(results$file, results$directory))
}


#' Upload a directory
#' @param path a scalar character vector containing a single directory path
#' @return An `osf_tbl_file` containing a single row for `path`'s leaf directory
#' @noRd
.upload_dir <- function(dest, path, conflicts, recurse, progress, verbose) {
  stopifnot(rlang::is_scalar_character(path))

  # memoise osf directory retrieval to avoid subsequent API calls for every
  # file or subdirectory contained therein
  get_path <- memoise::memoise(
    function(x, path, verbose) {
      recurse_path(x, path, missing_action = "create", verbose = verbose)
    }
  )
  on.exit(memoise::forget(get_path))

  out <- get_path(dest, path = basename(path), verbose = verbose)

  targets <- tibble::tibble(
    local = fs::dir_ls(path, type = c("file", "directory"), recurse = recurse)
  )

  # upload target files/directories to the root of the osf destination
  targets$remote <- fs::path_rel(targets$local, dirname(path))
  targets$destination <- lapply(dirname(targets$remote),
    get_path,
    x = dest,
    verbose = verbose
  )

  targets$type <- fs::file_info(targets$local)$type
  targets <- split(targets, targets$type, drop = TRUE)
  results <- vector(mode = "list")

  if (!is.null(targets$file)) {
    results$file <- Map(
      f = .upload_file,
      dest = targets$file$destination,
      path = targets$file$local,
      conflicts = conflicts,
      progress = progress,
      verbose = verbose
    )
  }

  if (!is.null(targets$directory)) {
    results$directory <- Map(
      f = get_path,
      path = targets$directory$remote,
      x = list(dest),
      verbose = verbose
    )
  }

  return(out)
}


#' Internal file upload function
#'
#' This is a non-vectorized function that uploads a single file at a time. It
#' handles the logic for uploading new files or updating existing ones.
#'
#' @param dest OSF node or directory upload destination
#' @param path scalar character vector with the path of the file to be uploaded
#' @return `osf_tbl_file` with a single row for the uploaded file
#' @noRd

.upload_file <- function(dest, path, conflicts, progress, verbose) {

  # force the uploaded filename to match the local filename
  filename <- basename(path)

  # set arguments depending on whether destination is a directory or node
  upload_args <- list(
    name = filename,
    body = crul::upload(path),
    progress = progress
  )

  if (inherits(dest, "osf_tbl_node")) {
    upload_args$id <- as_id(dest)
  } else {
    upload_args$id <- get_parent_id(dest)
    upload_args$fid <- as_id(dest)
  }

  if (progress) cat(sprintf("Uploading %s\n", filename))
  res <- do.call(".wb_file_upload", upload_args)

  if (is.null(res$errors)) {
    if (verbose) message(sprintf("Uploaded new file '%s' to OSF", filename))
  } else {

    # raise error as usual if error is anything other than 409 (file exists)
    if (res$status_code != 409) raise_error(res)
    if (conflicts == "error") stop_conflict(path, "upload")

    conflicting_file <- osf_find_file(dest, pattern = filename, type = "file")

    if (conflicts == "skip") {
      msg <- sprintf(
        "Skipping file '%s' to avoid overwriting the existing copy on OSF",
        filename
      )
      if (verbose) message(msg)
      return(conflicting_file)
    }

    # overwrite existing file
    upload_args$fid <- as_id(conflicting_file)
    upload_args$name <- NULL

    if (progress) cat(sprintf("Uploading file '%s'\n", filename))
    res <- do.call(".wb_file_update", upload_args)
    if (verbose) message(sprintf("Uploaded new version of '%s' to OSF", filename))
  }

  # the metadata returned by waterbutler is a subset of what osf provides
  # so this extra API call allows us to return a consistent osf_tbl_file
  file_id <- extract_osf_id(res$data$links$upload)
  out <- .osf_file_retrieve(file_id)

  as_osf_tbl(out["data"], subclass = "osf_tbl_file")
}


#' File transfer messages
#'
#' Generate messages about file conflicts while uploading or downloading.
#' @param filename Name of the file to be transferred
#' @param direction Label indicating whether the transfer was `"uploading"` or
#'   `"downloading`
#' @noRd

stop_conflict <- function(filename, direction) {
  msg <- bullet_msg(
    sprintf("Can't %s file '%s'.", direction, basename(filename)),
    c(
      "A file with the same name already exists at the destination.",
      "Use the `conflicts` argument to avoid this error in the future."
    )
  )
  abort(msg)
}

warn_ul_conflict <- function(filename) {
  msg <- bullet_msg(
    sprintf("Local file '%s' was NOT uploaded to OSF.", filename),
    c(
      "A file with the same name already exists at the destination.",
      "Use the `conflicts` argument to avoid this error in the future."
    )
  )
}

