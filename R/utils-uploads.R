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


#' Upload new file
#'
#' @param dest OSF node or directory upload destination
#' @param path scalar character vector with the path of the file to be uploaded
#' @return A list object containing the processed waterbutler json response
#' @noRd

.upload_new_file <- function(dest, path, progress, verbose) {
  stopifnot(inherits(dest, c("osf_tbl_node", "osf_tbl_file")))
  stopifnot(rlang::is_scalar_character(path))

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

  raise_error(res)
  if (verbose) message(sprintf("Uploaded new file '%s' to OSF", filename))
  res
}

#' Update existing file
#' @param dest An osf_tbl_file with the remote file to be overwritten
#' @param path scalar character vector with the path of the file to be uploaded
#' @return A list object containing the processed waterbutler json response
#' @noRd

.update_existing_file <- function(dest, path, progress, verbose) {
  stopifnot(inherits(dest, "osf_tbl_file"))
  stopifnot(rlang::is_scalar_character(path))

  # force the uploaded filename to match the local filename
  filename <- basename(path)

  if (progress) cat(sprintf("Uploading file '%s'\n", filename))
  res <- .wb_file_update(
    id = get_parent_id(dest),
    fid = as_id(dest),
    body = crul::upload(path),
    progress = progress
  )

  raise_error(res)
  if (verbose) message(sprintf("Uploaded new version of '%s' to OSF", filename))
  res
}
