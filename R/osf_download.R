#' Download files and directories from OSF
#'
#' @description
#' Files stored on OSF can be downloaded locally by providing an
#' [`osf_tbl_file`] that contains the OSF file of interest. If the
#' [`osf_tbl_file`] contains a directory, a zip file containing the
#' directory's contents will be downloaded.
#'
#' By default files are downloaded to your current working directory with the
#' same filename used on OSF. The `path` argument can be used to specify a
#' different destination *and*, optionally,  a different filename for the
#' downloaded file. Note, the directory portion of `path` must reference an
#' existing directory.
#'
#' OSF directories are downloaded as zip files and
#'
#' @param x An [`osf_tbl_file`] containing a single file or directory.
#' @param path Path pointing to a local directory where the downloaded files
#'   will be saved. Default is to use the current working directory.
#' @param conflicts This determines what happens when a file with the same name
#'   exists at the specified destination. Can be one of the following:
#'   * `"error"` (the default): throw an error and abort the file transfer operation.
#'   * `"skip"`: skip the conflicting file(s) and continue transferring the
#'     remaining files.
#'   * `"overwrite"`: replace the existing file with the transferred copy.
#' @template progress
#' @template verbose
#'
#' @return The [`osf_tbl_file`] input with a new column, `"local_path"`,
#'   containing the downloaded file's path.
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
      f =  download_dir,
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
      f =  download_file,
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
#' This is a non-vectorized function that downloads a single file. It
#' implements the strategies for handling conflicting files.
#'
#' @param file OSF file or directory
#' @param path scalar character vector with the complete path of the download
#'  destination (i.e., directory path and file path)
#' @return `osf_tbl_file` with a single row
#' @noRd

download_file <- function(x, path, conflicts, progress, verbose) {

  local_path <- file.path(clean_path(path), x$name)

  out <- tibble::add_column(
    .data = x,
    local_path = as.character(local_path),
    .before = "meta"
  )

  if (fs::file_exists(local_path)) {
    if (conflicts == "error") stop_dl_conflict(x$name)
    if (conflicts == "skip") {
      warn(sprintf(
        "\nSkipping file '%s' to avoid overwriting local copy.\n", filename
      ))
      return(out)
    }
  }

  .wb_download(
    id = get_parent_id(x),
    fid = as_id(x),
    path = local_path,
    type = "file",
    zip = FALSE,
    verbose = verbose,
    progress = progress
  )

  out
}


download_dir <- function(x, path, conflicts, recurse, progress, verbose) {

  local_path <- file.path(clean_path(path), x$name)

  out <- tibble::add_column(
    .data = x,
    local_path = local_path,
    .before = "meta"
  )

  # download zipped directory to a temp location
  zip_file <- fs::path(tempdir(), x$name, ext = "zip")

  # print a message because this can take a while on OSF's end
  message(sprintf("Requesting folder '%s' from OSF...", x$name))

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
  files <- data.frame(
    # 1. remote OSF path
    remote = fs::path_rel(unzipped, dirname(zip_file)),
    # 2. path to downloaded files temp location
    downloaded = unzipped,
    stringsAsFactors = FALSE
  )

  # 3. path to local destination
  files$destination <- fs::path(path, files$remote)

  # filter by recursion level
  files$levels <- purrr::map_int(fs::path_split(files$remote), length) - 1
  files <- subset(files, levels <= recurse)

  # handle conflicted files
  files$conflicted <- fs::file_exists(files$destination)

  if (any(files$conflicted)) {
    if (conflicts == "error") {
      stop_dl_conflict(files$remote[files$conflicted][1])
    } else if (conflicts == "skip") {
      if (verbose) {
        message(
          "Skipped the following file(s) to avoid overwriting local copies:\n",
          sprintf("  * %s\n", files$remote[files$conflicted])
        )
      } else {
        warn(sprintf(
          "Skipped %i file(s) to avoid overwriting local copies.\n",
          sum(files$conflicted)
        ))
      }
      files <- subset(files, !conflicted)
    }
  }

  # create destination directories and copy remaining unzipped files
  fs::dir_create(unique(dirname(files$destination)))
  files$copied <- fs::file_copy(files$downloaded, files$destination)

  if (verbose) {
    message(
      "Downloaded the following file(s) from OSF:\n",
      sprintf("  * %s\n", files$copied)
    )
  }

  return(out)
}


stop_dl_conflict <- function(filename) {
  abort(paste0(
    sprintf("Can't download file '%s' from OSF.\n", filename),
    "A file with the same name already exists in the specified path.\n",
    "  * Use the `conflicts` argument to avoid this error in the future.\n"
  ))
}
