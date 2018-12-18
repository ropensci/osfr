#' Upload to OSF
#'
#' @param id an \code{osf_tbl_node} or OSF node ID, or an \code{osf_tbl_file} containing a single folder or waterbutler folder ID
#' @param path Path to file on local machine to upload. Ensure file has
#' proper extension named (i.e., extension sensitive, not like on Linux)
#' @param name Name of the uploaded file (if \code{NULL},
#' \code{basename(path)} will be used).
#'
#' @return Waterbutler URL
#' @seealso \code{\link{upload_files}}, \code{\link{upload_revised_files}}
#' @importFrom crul upload

osf_upload <- function(id, path, name = NULL, overwrite = FALSE) {
  id <- as_id(id)
  typ <- process_type(id)
  if (typ != 'nodes') {
    stop('Cannot upload new file if no node ID is specified.')
  }

  if (!file.exists(path)) stop(sprintf("File does not exist:\n %s", path))
  if (is.null(name)) name <- basename(path)
  out <- .wb_file_upload(id, name, body = crul::upload(path))

  # the metadata returned by waterbutler is a subset of what's returned by osf
  # so this extra call to osf allows us to return a consistent osf_tbl_file
  file_id <- strsplit(out$data$id, split = "/", fixed = TRUE)[[1]][2]
  out <- .osf_file_retrieve(file_id)

  as_osf_tbl_file(out['data'])
}


