#' View on OSF
#'
#' View a project, component, file or user profile on osf.io with your default
#' web browser.
#'
#' @export
osf_open <- function(id) UseMethod("osf_open")

#' @export
osf_open.default <- function(id) {
  stop(sprintf("Not sure how to open entity of class: ", class(id)))
}

#' @export
osf_open.character <- function(id) osf_open(as_id(id))

#' @export
osf_open.osf_id <- function(id) browseURL(osf_url(id))

#' @export
osf_open.osf_tbl <- function(id) browseURL(id$meta[[1]]$links$html)

#' @export
osf_open.osf_tbl_file <- function(id) {
  # a guid is not assigned to a file until it has been viewed directly on OSF
  # so we manually construct a url that triggers the OSF's file view
  parent_id <- id$meta[[1]]$relationships$node$data$id
  provider <- id$meta[[1]]$attributes$provider
  file_url <- osf_url(sprintf("%s/files/%s/%s", parent_id, provider, id$id[1]))
  browseURL(file_url)
}


# OSF URL that accounts for $OSF_USE_SERVER
osf_url <- function(id = NULL) {
  server <- Sys.getenv('OSF_USE_SERVER')
  url <- if (nzchar(server)) {
    sprintf("https://%s.osf.io", server)
  } else {
    "https://osf.io"
  }
  if (!is.null(id)) url <- file.path(url, id)
  url
}
