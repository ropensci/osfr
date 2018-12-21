#' List files and folders
#'
#' List the files and folders in the top-level of an OSF Project or Component.
#'
#' Specify a \code{path} or a \code{path_id} to list the contents of a
#' particular subdirectory. If both \code{path} \emph{and} \code{path_id} are
#' defined, then \code{path} is assumed to be a subdirectory within the
#' directory corresponding to the supplied \code{path_id}.
#'
#' @param id OSF project/component GUID
#' @param path list files within the specified subdirectory path
#' @param path_id OSF unique identifier assigned to a directory
#' @param type filter response to include only \code{"files"} or
#'   \code{"folders"}
#' @param pattern filter response to include entities whose name contain the
#'   specified pattern
#' @template n_max
#'
#' @export

osf_ls <- function(id, path = NULL, path_id = NULL, type = "any", pattern = NULL, n_max = Inf) {

  # hack
  if (inherits(id, "osf_tbl_user") || type == "node") return(osf_node_ls(id, n_max))

  id <- as_id(id)

  if (is.null(path_id)) {
    url_path <- sprintf("nodes/%s/files/osfstorage/", id)
  } else {
    url_path <- sprintf("nodes/%s/files/osfstorage/%s/", id, path_id)
  }

  # TODO: filter processing should be handled in an external function
  filters <- list()
  if (type != "any") {
    type <- match.arg(type, c("file", "folder"))
    filters$kind <- type
  }
  if (is.character(pattern)) {
    filters$name <- pattern
  }

  if (!rlang::is_empty(filters)) {
    names(filters) <- sprintf("filter[%s]", names(filters))
  }

  items <- .osf_paginated_request(
    method = "get",
    path = osf_path(url_path),
    query = filters,
    n_max = n_max,
    verbose = FALSE
  )

  if (rlang::is_empty(items)) {
    out <- osf_tbl(subclass = "osf_tbl_file")
  } else {
    out <- as_osf_tbl(items, subclass = "osf_tbl_file")
  }

  # recurse if path contains subdirectories
  path <- path %||% "."
  if (path != ".") {
    # find result that matches the first-level of the specified path
    path_root <- fs::path_split(path)[[1]][1]
    path_id <- out$id[which(out$name == path_root)]
    if (length(path_id) == 0) stop("Path does not exist: ", path_root)
    out <- osf_ls(id, path = fs::path_rel(path, path_root), path_id = path_id)
  }
  out
}

#' Create a OSF folder
#'
#' Creates a new folder in an OSF project or component.
#'
#' @param id Parent OSF project id (osf.io/XXXXX; just XXXXX) to create folder in
#' @param path Name for the new folder
#' @param parent_id Target path for the new folder
#' @template verbose
#'
#' @return Waterbutler URL for folder "root", last subfolder "sub", or all
#' folders created "all" depanding on the selection input for \code{return}
#' @export
#' @examples
#' \dontrun{
#' proj <- osf_project("Making Directories")
#' osf_mkdir(proj, "data")
#' osf_mkdir(proj, "data/raw_data")
#' osf_mkdir(proj, "super_raw_data", parent_id = "5be5e6a6fe3eca00188178f0")
#' }

osf_mkdir <- function(id, path, parent_id = NULL, verbose = FALSE) {
  id <- as_id(id)
  path_root <- fs::path_split(path)[[1]][1]
  items <- osf_ls(id, path_id = parent_id)

  if (path_root %in% items$name) {
    parent_id <- items$id[which(items$name == path_root)]
    if (verbose) {
      message(
        sprintf("%s (%s) already exists in node %s", path_root, parent_id ,id))
    }
  } else {
    new_dir <- .wb_create_folder(id, path_root, parent_id)
    parent_id <- gsub("/", "", new_dir$data$attributes$path, fixed = TRUE)
    if (verbose) {
      message(
        sprintf("Created %s (%s) in node %s", path_root, parent_id ,id))
    }
  }

  # recurse to the next-level if there is a subfolder
  path_next <- fs::path_rel(path, path_root)
  if (path_next != ".") {
    if (verbose) {
      message(
        sprintf("Continuing from %s to the next subfolder: %s",
                path_root, path_next))
    }
    out <- osf_mkdir(id, path = path_next, parent_id = parent_id)
  } else {
    out <- as_osf_tbl(.osf_file_retrieve(parent_id)[1], "osf_tbl_file")
  }
  out
}
