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
