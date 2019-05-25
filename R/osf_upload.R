#' Upload files and directories to OSF
#'
#' Local files and directories can be uploaded to a project, component or directory on OSF.
#'
#' @section A note about synchronization:
#' While `osf_download()` and `osf_upload()` can be used to conveniently shuttle
#' files back and forth between OSF and your local machine, it's important to
#' note that **they are not file synchronization functions**. In contrast to
#' something like [`rsync`](https://rsync.samba.org),
#' `osf_download()`/`osf_upload()` do not take into account file contents or
#' file modification times. Whether you're uploading or downloading, if
#' `overwrite=TRUE`, osfr will overwrite an existing file regradless of whether
#' the existing file is the more recent copy. You have been warned.
#'
#' @section Uploading to subdirectories:
#' If you want to upload to an existing directory on OSF, you will first need to retrieve it. For example, if the project `proj` has a subdirectory, `rawdata/`, nested within a top-level directory, `data/`, `osf_ls_files()` could be used to retrieve `rawdata/` directly
#'
#' ```
#' rawdata_dir <- osf_ls_files(proj, path = "data", pattern = "rawdata")
#' osf_upload(rawdata_dir, path = "my-file.txt")
#' ```
#'
#' @param x an [`osf_tbl_node`] with a single project or
#'   component, or an [`osf_tbl_file`] with a single directory. This is the OSF
#'   bucket to which your files will be uploaded.
#' @param path A character vector of filepaths to upload to `x` on OSF. Only
#'   paths pointing to existing files and/or directories are uploaded. Any other
#'   file types (e.g., symlinks) are silently ignored.
#' @param overwrite Logical, overwrite an existing file with the same name
#'   (default `FALSE`)? If `TRUE`, OSF will automatically update the file and
#'   record the previous version. If `FALSE`, a warning will be issued that the
#'   local file was *not* uploaded and the *existing* version of the file on OSF
#'   is returned.
#' @template verbose
#'
#' @return an [`osf_tbl_file`] containing only the files/directories uploaded to
#'   the root-level of `x`. It will not include the files/directories that were
#'   uploaded to subdirectories within `x`.
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
#' @importFrom fs is_file is_dir path_dir file_info dir_walk

osf_upload <-
  function(x,
           path,
           overwrite = FALSE,
           verbose = FALSE) {
  UseMethod("osf_upload")
}

#' @export
osf_upload.osf_tbl_node <-
  function(x,
           path,
           overwrite = FALSE,
           verbose = FALSE) {

  path <- check_files(path)
  x <- make_single(x)
  recursive_upload(x, path, overwrite, verbose)
}

#' @export
osf_upload.osf_tbl_file <-
  function(x,
           path,
           overwrite = FALSE,
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

  recursive_upload(x, path, overwrite, verbose)
}


#' @param dest OSF node or directory upload destination

recursive_upload <- function(dest, path, overwrite, verbose) {

  # memoise osf directory retrieval to avoid subsequent API calls for every
  # file or subdirectory contained therein
  get_path <- memoise::memoise(
    function(x, path) {
      message(sprintf("Getting path `%s` from `%s`", path, as_id(x)))
      recurse_path(x, path, missing_action = "create", verbose = verbose)
    }
  )

  # split into top-level files and folders
  path_by <- split(path, fs::file_info(path)$type, drop = TRUE)

  # upload files in pwd
  if (is.null(path_by$file)) {
    out_files <- NULL
  } else {
    out_files <- map(
      .x  = path_by$file,
      .f = upload_file,
      dest = dest,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  # recurse directories in pwd
  if (is.null(path_by$directory)) {
    out_dirs <- NULL
  } else {
    out_dirs <- fs::dir_walk(path_by$directory, recursive = TRUE, fun = function(file) {
        message(sprintf("Working on %s", file))

        # if path is a directory, create or retrieve the corresponding osf folder
        # and return it. if path is a file, retrieve its osf parent.
        if (fs::is_dir(file)) {
          get_path(dest, file)
        } else {
          parent_path <- dirname(file)
          if (parent_path == ".") {
            parent <- dest
          } else {
            parent <- get_path(dest, parent_path)
          }
          upload_file(parent, file, overwrite = overwrite, verbose = verbose)
        }
      }
    )
    # dir_walk() only returns directory names, so we need to re-retrieve them
    out_dirs <- map(out_dirs, get_path, x = dest)
  }

  memoise::forget(get_path)
  do.call("rbind", c(out_files, out_dirs))
}


#' Internal file upload function
#'
#' This is a non-vectorized function that uploads a single file at a time. It
#' handles the logic for uploading new files or updating existing ones.
#'
#' @param dest OSF node or directory upload destination
#' @param path scalar character vector with the path of the file to be uploaded
#' @return `osf_tbl_file` with a single row
#' @noRd

upload_file <- function(dest, path, overwrite, verbose) {

  # force the uploaded filename to match the local filename
  name <- basename(path)

  # set arguments depending on whether destination is a directory or node
  upload_args <- list(name = name, body = crul::upload(path))

  if (inherits(dest, "osf_tbl_node")) {
    upload_args$id <- as_id(dest)
  } else {
    upload_args$id <- get_parent_id(dest)
    upload_args$fid <- as_id(dest)
  }

  res <- do.call(".wb_file_upload", upload_args)

  if (is.null(res$errors)) {
    if (verbose) message(sprintf("Uploaded new file %s to OSF", basename(path)))
  } else {
    # raise error as usual if error is anything other than 409 (file exists)
    if (res$status_code != 409) raise_error(res)

    # retrieve existing file from osf
    osf_items <- osf_ls_files(dest, type = "file", pattern = name)
    osf_file <- osf_items[osf_items$name == name, ]

    if (overwrite) {
      upload_args$fid <- as_id(osf_file)
      upload_args$name <- NULL
      res <- do.call(".wb_file_update", upload_args)
      if (verbose) message(sprintf("Uploaded new version of %s to OSF", name))
    } else {
      warn(paste0(
        sprintf("Local file '%s' was NOT uploaded to OSF.\n", name),
        "A file with the same name already exists in that location and 'ovewrite = FALSE'\n",
        "  * The current OSF version of this file will be returned instead\n",
        "  * Set 'overwrite = TRUE' and re-upload to create a new version on OSF"
      ))
      return(osf_file)
    }
  }

  # the metadata returned by waterbutler is a subset of what osf provides
  # so this extra API call allows us to return a consistent osf_tbl_file
  file_id <- strsplit(res$data$id, split = "/", fixed = TRUE)[[1]][2]
  out <- .osf_file_retrieve(file_id)

  as_osf_tbl(out["data"], subclass = "osf_tbl_file")
}
