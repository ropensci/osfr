#' Upload to OSF
#'
#' @param id OSF project id (osf.io/XXXXX) to upload to.
#' @param path Path to file on local machine to upload. Ensure file has
#' proper extension named (i.e., extension sensitive, not like on Linux)
#' @param name Name of the uploaded file (if \code{NULL},
#' \code{basename(path)} will be used).
#'
#' @return Waterbutler URL
#' @seealso \code{\link{upload_files}}, \code{\link{upload_revised_files}}
#' @importFrom crul upload

osf_upload <- function(id, path, name = NULL, overwrite = FALSE) {

  typ <- process_type(id)
  if (typ != 'nodes') {
    stop('Cannot upload new file if no node ID is specified.')
  }

  if (!file.exists(path))
    stop(sprintf('File %s does not exist on local machine.', path))

  if (is.null(name)) name <- basename(path)
  query <- list(kind = "file", name = name)

  cli <- wb_cli()
  res <- cli$put(wb_path(id), query = query, body = crul::upload(path))

  out <- jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
  if (res$status_code == 409) http_error(res$status_code, out$message)
  res$raise_for_status()

  structure(
    .Data = out$data$id,
    links = out$data$links,
    attributes = out$data$attributes,
    class = paste0("osf_", out$data$attributes$kind)
  )
}


