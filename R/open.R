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
osf_open.osf_id <- function(id) utils::browseURL(osf_url(id))

#' @export
osf_open.osf_tbl <- function(id) utils::browseURL(id$meta[[1]]$links$html)


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
