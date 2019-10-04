#' @importFrom fs path_common
inform_dl_conflicts <- function(filenames, verbose) {
  stopifnot(is.logical(verbose))
  msg <- sprintf(
    "Skipped %i file(s) from OSF folder '%s' to avoid overwriting local copies",
    length(filenames),
    fs::path_common(filenames)
  )
  if (verbose) msg <- bullet_msg(paste0(msg, ":"), filenames)
  message(msg)
}


stop_dl_conflict <- function(filename) {
  msg <- bullet_msg(
    sprintf("Can't download file '%s' from OSF.", filename),
    "Use the `conflicts` argument to avoid this error in the future."
  )
  abort(msg)
}
