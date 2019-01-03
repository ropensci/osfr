#' View on OSF
#'
#' View a project, component, file or user profile on osf.io with your default
#' web browser.
#'
#' @template input-osf-entity
#'
#' @export
osf_open <- function(x) UseMethod("osf_open")

#' @export
osf_open.default <- function(x) {
  abort(sprintf("Not sure how to open entity of class: %s", class(x)))
}

#' @export
osf_open.character <- function(x) osf_open(as_id(x))

#' @export
osf_open.osf_id <- function(x) browseURL(osf_url(x))

#' @export
osf_open.osf_tbl <- function(x) browseURL(x$meta[[1]]$links$html)

#' @export
osf_open.osf_tbl_file <- function(x) {
  parent_id <- get_parent_id(x)
  if (is_osf_dir(x)) {
    # there is no directory view so we redirect to the parent node's file view
    url <- osf_url(sprintf("%s/files/", parent_id))
  } else {
    # a guid is not assigned to a file until it has been viewed directly on OSF
    # so we manually construct a url that triggers the OSF's file view
    provider <- get_meta(x, "attributes", "provider")
    url <- osf_url(sprintf("%s/files/%s/%s", parent_id, provider, as_id(x)))
  }
  browseURL(url)
}


# OSF URL that accounts for $OSF_USE_SERVER
osf_url <- function(id = NULL) {
  server <- Sys.getenv("OSF_USE_SERVER")
  url <- if (nzchar(server)) {
    sprintf("https://%s.osf.io", server)
  } else {
    "https://osf.io"
  }
  if (!is.null(id)) url <- file.path(url, id)
  url
}
