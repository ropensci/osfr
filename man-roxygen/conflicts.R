#' @param conflicts This determines what happens when a file with the same name
#'   exists at the specified destination. Can be one of the following:
#'   * `"error"` (the default): throw an error and abort the file transfer operation.
#'   * `"skip"`: skip the conflicting file(s) and continue transferring the
#'     remaining files.
#'   * `"overwrite"`: replace the existing file with the transferred copy.
