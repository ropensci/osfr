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
  if (!nchar(x) %in% c(5, 24)) abort("`x` is not a valid OSF ID.")
  structure(x, class = "osf_id")
}

as_id.osf_tbl_node <- function(x) as_id(x$id)
as_id.osf_tbl_file <- function(x) as_id(x$id)
as_id.osf_tbl_user <- function(x) as_id(x$id)
as_id.osf_id <- function(x) x


#' Determine OSF ID entity type
#' @param id OSF or waterbutler ID
#' @return files, nodes, or users
#' @noRd
id_type <- function(id) {

  # is it a waterbutler ID?
  if (nchar(id) == 24) return("files")

  out <- .osf_node_retrieve(id)
  if (is.null(out$errors)) return(out$data$type)

  out <- .osf_file_retrieve(id)
  if (is.null(out$errors)) return(out$data$type)

  out <- .osf_user_retrieve(id)
  if (is.null(out$errors)) return(out$data$type)

  stop("No OSF entity found for ID ", id)
}


