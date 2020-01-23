#' Upload files to OSF
#'
#' Upload local files to a project, component, or directory on OSF.
#'
#' @param x The upload destination on OSF. Can be one of the following:
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
#' Likewise, `osf_upload(my_proj, path = ".")` will upload your entire current
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
#' @importFrom purrr map_lgl
#' @noRd

.osf_upload <- function(dest, path, recurse, conflicts, progress, verbose) {

  # be more verbose when uploading directories
  any_dirs <- any(fs::is_dir(path))

  # inventory of files to upload and/or remote directories to create
  manifest <- map_rbind(.upload_manifest, path = path, recurse = recurse)

  # retrieve remote destinations
  manifest <- .ulm_add_remote_dests(manifest, dest, verbose)

  # identify conflicting files at remote destinations
  manifest <- .ulm_add_conflicting_files(manifest, verbose || any_dirs)
  manifest$conflicted <-  purrr::map_lgl(manifest$remote_file, Negate(is.null))

  # list of osf_tbls to be returned
  out <- list(
    dirs = NULL,
    skipped = NULL,
    updated = NULL,
    uploaded = NULL
  )

  # assemble directories specified in `path` for output
  path_dirs <- setdiff(intersect(manifest$remote_dir, basename(path)), ".")
  if (!is_empty(path_dirs)) {
    out$dirs <- do.call(
      "rbind",
      manifest$remote_dest[match(path_dirs, manifest$remote_dir)]
    )
  }

  # process target files in upload manifest
  manifest <- manifest[manifest$type == "file", ]

  if (any(manifest$conflicted)) {

    if (conflicts == "error") {
      stop_conflict(manifest$path[manifest$conflicted][1], "upload")
    }

    if (conflicts == "skip") {
      message(sprintf(
        "Skipped %i file(s) to avoid overwriting OSF copies",
        sum(manifest$conflicted)
      ))

      # drop skipped files from manifest and add them to output
      out$skipped <- do.call("rbind", manifest$remote_file[manifest$conflicted])
      manifest <- manifest[!manifest$conflicted, ]
    }
  }

  # split file inventory based on whether they will be uploaded or updated
  manifest <- split(
    manifest,
    ifelse(manifest$conflicted, "update", "upload")
  )

  if (!is.null(manifest$update)) {
    n <- nrow(manifest$update)
    msg <- sprintf("Updating %i existing file(s) on OSF", n)
    if (verbose || any_dirs) message(msg)

    out$updated <- Map(.update_existing_file,
      path = manifest$update$path,
      dest = manifest$update$remote_file,
      progress = progress,
      verbose = verbose
    )
  }

  if (!is.null(manifest$upload)) {
    n <- nrow(manifest$upload)
    msg <- sprintf("Uploading %i new file(s) to OSF", n)
    if (verbose || any_dirs) message(msg)

    out$uploaded <- Map(.upload_new_file,
      path = manifest$upload$path,
      dest = manifest$upload$remote_dest,
      progress = progress,
      verbose = verbose
    )
  }

  # return osf_tbls only for files passed directly to `path`
  out$skipped  <- out$skipped[out$skipped$name %in% basename(path), ]

  out$updated  <- out$updated[names(out$updated) %in% path]
  out$uploaded <- out$uploaded[names(out$uploaded) %in% path]

  out$updated  <- map_rbind(wb2osf, out$updated)
  out$uploaded <- map_rbind(wb2osf, out$uploaded)

  do.call("rbind", out)
}

