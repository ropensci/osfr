#' Download files from the OSF
#'
#' @param id Specify the node id (osf.io/XXXX)
#' @param file Specify path to save file to. If NULL, defaults to OSF filename
#' @param public Boolean to specify whether file is public
#'
#' @return Return filepath for easy processing
#' @export
#'
#' @examples
#' \dontrun{download_osf('zevw2', 'test123.md')}
download <- function(
  id = NULL,
  file = NULL,
  public = TRUE,
  ...) {

  if (is.null(id))
    stop("Enter node to download.")

  url.osf <- construct_link(sprintf("guids/%s", id), ...)

  if (public == FALSE) {
    if (Sys.getenv("OSF_PAT") == "")
      stop("Requires login, use login()")

    call <- httr::GET(
      url.osf,
      httr::add_headers(Authorization = sprintf("Bearer %s", login())))
  } else {
    call <- httr::GET(url.osf)
  }

  if (!call$status_code == 200)
    stop("Failed. Sure you have access to the file?")

  res <- process_json(call)

  # Find the filename as on the OSF
  if (is.null(file)) {
    txt <- res$data$attributes$name
    start <- tail(gregexpr("/", txt)[[1]], 1)
    end <-  nchar(txt)
    file <- substr(txt, start + 1, end)
  }

  cat(sprintf("Saving to filename: %s\n", file))

  if (public == TRUE) {
    call <- httr::GET(
      res$data$links$download,
      httr::write_disk(file, overwrite = TRUE))
  } else {
    call <- httr::GET(
      res$data$links$download,
      httr::add_headers(Authorization = sprintf("Bearer %s", login())),
      httr::write_disk(file, overwrite = TRUE))
  }

  if (call$status_code != 200)
    stop("Failed to download file.")

  cat("Successfully downloaded file.\n")

  return(file)
}
