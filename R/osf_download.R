#' Download files and directories from OSF
#'
#' @description
#' Files stored on OSF can be downloaded locally by passing an [`osf_tbl_file`]
#' that contains the files and folders of interest. Use `path` to specify
#' *where* the files should be downloaded, otherwise they are downloaded to
#' your working directory by default.
#'
#' @param x An [`osf_tbl_file`] containing a single file or directory.
#' @param path Path pointing to a local directory where the downloaded files
#'   will be saved. Default is to use the current working directory.
#' @template conflicts
#' @param recurse Applies only to OSF directories. If `TRUE`, a directory is
#'   fully recursed and all nested files and subdirectories are downloaded.
#'   Alternatively, a positive number will determine the number of levels
#'   to recurse.
#' @template progress
#' @template verbose
#'
#' @return The same [`osf_tbl_file`] passed to `x` with a new column,
#'   `"local_path"`, containing paths to the local files.
#'
#' @section Implementation details:
#' Directories are always downloaded from OSF as zip files that contain its
#' entire contents. The logic for handling conflicts and recursion is
#' implemented locally, acting on these files in a temporary location and
#' copying them to `path` as needed. This creates a *gotcha* if you're
#' downloading directories with large files and assuming that setting `conflicts
#' = "skip"` and/or limiting recursion will reduce the number of files you're
#' downloading. In such a case, a better strategy would be to use
#' `osf_ls_files()` to list the contents of the directory and pass that output
#' to `osf_download()`.
#'
#' @template synchronization
#'
#' @examples
#' \dontrun{
#' # download a single file
#' analysis_plan <- osf_retrieve_file("2ryha") %>%
#'   osf_download()
#'
#' # verify the file was downloaded locally
#' file.exists(analysis_plan$local_path)
#' }
#' @seealso
#' * [`osf_upload()`] for uploading files to OSF.
#' * [`osf_ls_files()`] for listing files and directories on OSF.
#'
#' @export
#' @importFrom fs path_ext_set path_ext_remove

osf_download <-
  function(x,
           path = NULL,
           recurse = FALSE,
           conflicts = "error",
           verbose = FALSE,
           progress = FALSE) {
    UseMethod("osf_download")
  }

#' @export
osf_download.osf_tbl_file <-
  function(x,
           path = NULL,
           recurse = FALSE,
           conflicts = "error",
           verbose = FALSE,
           progress = FALSE) {
    path <- check_local_dir(path)

    # convert recurse to number of recursion levels
    if (is.logical(recurse)) {
      recurse <- ifelse(isTRUE(recurse), Inf, 1)
    }

    requests <- split(x, ifelse(is_osf_dir(x), "folder", "file"))
    results <- vector(mode = "list")

    if (!is.null(requests$folder)) {
      n <- nrow(requests$folder)
      results$folder <- Map(
        f = .download_dir,
        x = split(requests$folder, seq_len(n)),
        path = path,
        conflicts = conflicts,
        recurse = recurse,
        progress = progress,
        verbose = verbose
      )
    }

    if (!is.null(requests$file)) {
      n <- nrow(requests$file)
      results$file <- Map(
        f = .download_file,
        x = split(requests$file, seq_len(n)),
        path = path,
        conflicts = conflicts,
        progress = progress,
        verbose = verbose
      )
    }

    # bind results within each type before returning merged file/folder slots
    results <- lapply(results, do.call, what = "rbind")
    do.call("rbind", results)
  }


#' Internal download functions
#'
#' These are non-vectorized functions for downloading a file or directory that
#' implement the logic for handling local conflicts.
#'
#' @param file OSF file or directory
#' @param path scalar character vector with the complete path of the download
#'  destination (i.e., directory path and file path)
#' @return `osf_tbl_file` with a single row for each file.
#'   * For `.download_file()` the result always contains a single row
#'   * For `.download_dir()` the result will contain 1 row for every file
#'     contained within the OSF directory.
#' @noRd

.download_file <- function(x, path, conflicts, progress, verbose) {
  local_path <- file.path(clean_local_path(path), x$name)

  out <- tibble::add_column(
    .data = x,
    local_path = as.character(local_path),
    .before = "meta"
  )

  if (fs::file_exists(local_path)) {
    if (conflicts == "error") stop_dl_conflict(x$name)
    if (conflicts == "skip") {
      msg <- sprintf(
        "Skipping file '%s' to avoid overwriting local copy",
        x$name
      )
      if (verbose) message(msg)
      return(out)
    }
  }

  .wb_download(
    id = get_parent_id(x),
    fid = as_wb_id(x),
    path = local_path,
    type = "file",
    zip = FALSE,
    verbose = verbose,
    progress = progress
  )

  out
}


.download_dir <- function(x, path, conflicts, recurse, progress, verbose) {
  local_path <- file.path(clean_local_path(path), x$name)

  out <- tibble::add_column(
    .data = x,
    local_path = local_path,
    .before = "meta"
  )

  # download zipped directory to a temp location
  zip_file <- fs::path(tempdir(), x$name, ext = "zip")

  # print a message because this can take a while on OSF's end
  message(sprintf("Requesting folder '%s' from OSF", x$name))

  .wb_download(
    id = get_parent_id(x),
    fid = as_id(x),
    path = zip_file,
    type = "folder",
    zip = TRUE,
    # hardcoded as FALSE to avoid messages about downloading the zip file
    verbose = FALSE,
    progress = progress
  )

  unzipped <- unzip(
    zip_file,
    overwrite = TRUE,
    exdir = fs::path_ext_remove(zip_file)
  )

  # each file has 3 location attributes:
  # 1. remote: OSF path
  # 2. downloaded: path to downloaded files temp location
  # 3. destination: path to local destination

  targets <- tibble::tibble(
    remote = fs::path_rel(unzipped, dirname(zip_file)),
    downloaded = unzipped
  )

  targets$destination <- fs::path(path, targets$remote)

  # filter by recursion level
  targets$levels <- purrr::map_int(fs::path_split(targets$remote), length) - 1
  targets <- targets[targets$levels <= recurse, ]

  # handle conflicted files
  targets$conflicted <- fs::file_exists(targets$destination)

  if (any(targets$conflicted)) {
    if (conflicts == "error") {
      stop_dl_conflict(targets$remote[targets$conflicted][1])
    } else if (conflicts == "skip") {
      inform_dl_conflicts(targets$remote[targets$conflicted], verbose)
      targets <- targets[!targets$conflicted, ]
    }
  }

  # create destination directories and copy remaining unzipped files
  fs::dir_create(unique(dirname(targets$destination)))
  targets$copied <- fs::file_copy(
    targets$downloaded,
    targets$destination,
    overwrite = TRUE
  )

  msg <- sprintf(
    "Downloaded %s file(s) from OSF folder '%s'",
    nrow(targets),
    x$name
  )
  if (verbose && nrow(targets) > 0) {
    msg <- bullet_msg(paste0(msg, ":"), targets$destination)
  }
  message(msg)
  return(out)
}