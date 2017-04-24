#' Processing a file id to waterbutler
#'
#' @param id OSF id (osf.io/xxxx)
#' @param private Boolean, if file is private
#'
#' @return Waterbutler URL

process_file_id <- function(id = NULL, private = FALSE, ...) {

  url.osf <- construct_link(sprintf("files/%s", id), ...)
  call <- httr::GET(url = url.osf)
  res <- process_json(call)

  # Choosing different links does not really matter
  # are equal
  return(res$data$links$download)
}
