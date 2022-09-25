#' Extract metadata values from OSF entity lists
#'
#' @param x an [`osf_tbl`] object
#' @param ... list of accessors passed to [`purrr::chuck()`]
#'
#' @examples
#' me <- osf_retrieve_user("me")
#' get_meta(me, "attributes", "full_name")
#' @noRd
get_meta <- function(x, ...) {
  stopifnot(inherits(x, "osf_tbl"))
  stopifnot(is.list(x$meta))
  out <- lapply(x$meta, purrr::chuck, ...)
  unlist(out, use.names = FALSE, recursive = FALSE)
}


# Extract relationships from an OSF entity
get_relation <- function(x, field) {
  fields <- list(
    files = function(x) x$relationships$files$links$related$href,
    root = function(x) x$relationships$node$data$id
  )
  stopifnot(field %in% names(fields))
  purrr::chuck(x$meta[[1]], fields[[field]])
}

# Retrieve the parent node's GUID for a component, file or directory
get_parent_id <- function(x) {
  stopifnot(inherits(x, c("osf_tbl_file", "osf_tbl_node")))
  stopifnot(nrow(x) == 1)

  parent_id <- switch(class(x)[1],
    osf_tbl_file = function(x) x$meta[[1]]$relationships$node$data$id,
    osf_tbl_node = function(x) x$meta[[1]]$relationships$parent$data$id
  )
  purrr::chuck(x, parent_id)
}

#' Remote path for node, file, or directory
#'
#' Details:
#'  - leading slash is always removed to facilitate comparisons with local files
#'  - directories always end with a trailing slash
#'  - '.' is returned for nodes since they represent the project root
#' @noRd
get_remote_path <- function(x) {
  stopifnot(inherits(x, c("osf_tbl_file", "osf_tbl_node")))

  get_path <- switch(class(x)[1],
    osf_tbl_file = function(x) get_meta(x, "attributes", "materialized_path"),
    osf_tbl_node = function(x) rep(".", nrow(x))
  )
  sub("^\\/", "", get_path(x))
}

#' Path for waterbutler API calls
#'
#' @noRd
get_waterbutler_path_id <- function(x) {
  stopifnot(inherits(x, c("osf_tbl_file", "osf_tbl_node")))

  get_wb_path <- switch(class(x)[1],
    osf_tbl_file = function(x) get_meta(x, "attributes", "path"),
    osf_tbl_node = function(x) rep(".", nrow(x))
  )

  sub("^\\/", "", get_wb_path(x))
}
