#' Extract OSF identifiers
#'
#' Retrieves unique identifiers from OSF objects.
as_id <- function(x) UseMethod("as_id")

as_id.character <- function(x) {

  # extract GUID from URLs
  if (grepl("osf.io", x, fixed = TRUE)) {
    x <- gsub("/", "", crul::url_parse(x)$path, fixed = TRUE)
  }

  # verify length is consistent with OSF GUID or waterbutler identifier
  if (!nchar(x) %in% c(5, 24)) stop(sprintf("%s is not a valid ID."))
  structure(x, class = "osf_id")
}

as_id.osf_tbl_file <- function(x) as_id(x$id)
as_id.osf_tbl_node <- function(x) as_id(x$id)
as_id.osf_id <- function(x) x
