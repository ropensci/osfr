# Extract relationships from an OSF entity
get_relation <- function(x, field) {
  fields <- list(
    files = function(x) x$relationships$files$links$related$href,
    root = function(x) x$relationships$node$data$id
  )
  stopifnot(field %in% names(fields))
  purrr::pluck(x$meta[[1]], fields[[field]])
}


# Extract attributes from an OSF entity
get_attr <- function(x, field) {
  fields <- list(
    kind = function(x) x$attributes$kind
  )
  stopifnot(field %in% names(fields))
  purrr::pluck(x$meta[[1]], fields[[field]])
}


# Extract links from an OSF entity
get_link <- function(x, field) {
  fields <- list(
    info = function(x) x$links$info,
    new_folder = function(x) x$links$new_folder,
    move = function(x) x$links$move,
    upload = function(x) x$links$upload,
    delete = function(x) x$links$delete
  )
  stopifnot(field %in% names(fields))
  purrr::pluck(x$meta[[1]], fields[[field]])
}


# Retrieve the parent node's GUID for a component, file or directory
get_parent_id <- function(x) {
  stopifnot(inherits(x, "osf_tbl"))
  parent_id <- switch(class(x)[1],
    osf_tbl_file = function(x) x$meta[[1]]$relationships$node$data$id,
    osf_tbl_node = function(x) x$meta[[1]]$relationships$parent$data$id
  )
  purrr::pluck(x, parent_id)
}
