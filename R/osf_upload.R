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
#' @param overwrite Logical, overwrite an existing file with the same name
#'   (default `FALSE`)? If `TRUE`, OSF will automatically update the file and
#'   record the previous version. If `FALSE`, a warning will be issued that the
#'   local file was *not* uploaded and the *existing* version of the file on OSF
#'   is returned.
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
#' directory to `path` instead of the file itself:
#'
#' ```
#' osf_upload(my_proj, c("a.txt", "subdir2"))
#' ```
#'
#' These behaviors are intended to to make it convenient to replicate your
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
#' @section A note about synchronization:
#' While `osf_download()` and `osf_upload()` can be used to conveniently shuttle
#' files back and forth between OSF and your local machine, it's important to
#' note that **they are not file synchronization functions**. In contrast to
#' something like [`rsync`](https://rsync.samba.org),
#' `osf_download()`/`osf_upload()` do not take into account a file's contents or
#' modification time. Whether you're uploading or downloading, if `overwrite =
#' TRUE`, osfr will overwrite an existing file regradless of whether the
#' existing file is the more recent copy. You have been warned.
#'
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
           overwrite = FALSE,
           progress = FALSE,
           verbose = FALSE) {
  UseMethod("osf_upload")
}

#' @export
osf_upload.osf_tbl_node <-
  function(x,
           path,
           recurse = FALSE,
           overwrite = FALSE,
           progress = FALSE,
           verbose = FALSE) {

  path <- check_files(path)
  x <- make_single(x)
  recursive_upload(x, path, recurse, overwrite, progress, verbose)
}

#' @export
osf_upload.osf_tbl_file <-
  function(x,
           path,
           recurse = FALSE,
           overwrite = FALSE,
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

  recursive_upload(x, path, recurse, overwrite, progress, verbose)
}


#' Recursive file and directory upload function
#'
#' This function maps over all elements in `path` and recursively walks any
#' subdirectories, calling `file_upload()` for each file it counters..
#' @param dest OSF node or directory upload destination
#' @importFrom fs is_dir file_info dir_walk
#' @noRd

recursive_upload <- function(dest, path, recurse, overwrite, progress, verbose) {

  # memoise osf directory retrieval to avoid subsequent API calls for every
  # file or subdirectory contained therein
  get_path <- memoise::memoise(
    function(x, path) {
      recurse_path(x, path, missing_action = "create", verbose = verbose)
    }
  )
  on.exit(memoise::forget(get_path))

  # split into top-level files and folders
  path_by <- split(path, fs::file_info(path)$type, drop = TRUE)

  # upload files in pwd
  if (is.null(path_by$file)) {
    out_files <- NULL
  } else {
    out_files <- map(path_by$file,
      .f = upload_file,
      dest = dest,
      overwrite = overwrite,
      progress = progress,
      verbose = verbose
    )
  }

  # recurse directories in pwd
  if (is.null(path_by$directory)) {
    out_dirs <- NULL
  } else {
    out_dirs <- fs::dir_walk(
      path,
      type = c("file", "directory"),
      recurse = recurse,
      function(p) {
        # * if path is a dir, [create and] retrieve the corresponding osf dir
        # * if path is a file, upload to its parent dir on osf
        if (fs::is_dir(p)) {
          get_path(dest, p)
        } else {
          parent_path <- dirname(p)
          if (parent_path == ".") {
            parent <- dest
          } else {
            parent <- get_path(dest, parent_path)
          }
          upload_file(parent, path = p, overwrite, progress, verbose)
        }
      }
    )
    # dir_walk() only returns directory names so we need to re-retrieve them
    out_dirs <- map(out_dirs, get_path, x = dest)
  }

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

upload_file <- function(dest, path, overwrite, progress, verbose) {

  # force the uploaded filename to match the local filename
  name <- basename(path)

  # set arguments depending on whether destination is a directory or node
  upload_args <- list(
    name = name,
    body = crul::upload(path),
    progress = progress
  )

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
      if (verbose) message(sprintf("Uploaded new version of '%s' to OSF", name))
    } else {
      warning(paste0(
        sprintf("\nLocal file '%s' was NOT uploaded to OSF.\n", name),
        "A file with the same name already exists in that location and 'ovewrite = FALSE'\n",
        "  * The current OSF version of this file will be returned instead\n",
        "  * Set 'overwrite = TRUE' and re-upload to create a new version on OSF\n"
        ),
        immediate. = TRUE,
        call. = FALSE
      )
      return(osf_file)
    }
  }

  # the metadata returned by waterbutler is a subset of what osf provides
  # so this extra API call allows us to return a consistent osf_tbl_file
  file_id <- strsplit(res$data$id, split = "/", fixed = TRUE)[[1]][2]
  out <- .osf_file_retrieve(file_id)

  as_osf_tbl(out["data"], subclass = "osf_tbl_file")
}
