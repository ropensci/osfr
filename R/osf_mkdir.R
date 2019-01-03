#' Create a OSF folder
#'
#' Creates a new folder in an OSF project or component.
#'
#' @param x an [`osf_tbl_node`] representing an OSF project or component or an
#'   [`osf_tbl_file`] containing a directory
#' @param path Name for the new directory
#' @template verbose
#'
#' @return an [`osf_tbl_file`] containing the last directory specified in `path`
#' @export
#' @examples
#' \dontrun{
#' proj <- osf_create_project("Directory Example")
#' osf_mkdir(proj, "data/raw_data")
#' }

osf_mkdir <- function(x, path, verbose = FALSE) {
  UseMethod("osf_mkdir")
}

# TODO: DRY out the osf_tbl_node and osf_tbl_file methods

#' @export
osf_mkdir.osf_tbl_node <- function(x, path, verbose = FALSE) {

  x <- make_single(x)
  id <- as_id(x)

  # does path root already exist?
  path_root <- fs::path_split(path)[[1]][1]
  items <- osf_ls_files(x, type = "folder", pattern = path_root)
  dir_root <- items[which(items$name == path_root), ]

  if (nrow(dir_root) == 0) {
    res <- .wb_create_folder(id = id, name = path_root)
    raise_error(res)
    dir_id <- gsub("/", "", res$data$attributes$path, fixed = TRUE)
    dir_root <- osf_retrieve_file(dir_id)
    msg <- sprintf("Created %s (%s) in node %s", path_root, dir_id, id)
  } else {
    msg <- sprintf("%s already exists in node %s", path_root, id)
  }

  if (verbose) message(msg)

  # recurse to the next-level if there is a subfolder
  path_next <- fs::path_rel(path, path_root)
  if (path_next == ".") {
    out <- dir_root
  } else {
    msg <- sprintf("Continuing from %s to the next subfolder: %s", path_root, path_next)
    if (verbose) message(msg)
    out <- osf_mkdir(dir_root, path_next, verbose)
  }
  out
}

#' @export
osf_mkdir.osf_tbl_file <- function(x, path, verbose = FALSE) {
  x <- make_single(x)
  id <- as_id(x)
  if (is_osf_file(x)) abort("Can't create directories within a file.")

  # does path root already exist?
  path_root <- fs::path_split(path)[[1]][1]
  items <- osf_ls_files(x, type = "folder", pattern = path_root)
  dir_root <- items[which(items$name == path_root), ]

  if (nrow(dir_root) == 0) {
    res <- .wb_create_folder(id = get_parent_id(x), name = path_root, fid = id)
    raise_error(res)

    dir_id <- gsub("/", "", res$data$attributes$path, fixed = TRUE)
    dir_root <- osf_retrieve_file(dir_id)
    msg <- sprintf("Created subdirectory %s (%s) in directory %s (%s)",
                   path_root, dir_id, x$name, id)
  } else {
    msg <- sprintf("Subdirectory %s (%s) already exists in directory %s (%s)",
                   path_root, items$id, x$name, id)
  }

  if (verbose) message(msg)

  # recurse to the next-level if there is a subfolder
  path_next <- fs::path_rel(path, path_root)
  if (path_next == ".") {
    out <- dir_root
  } else {
    msg <- sprintf("Continuing from %s to the next subdirectory: %s",
                   path_root, path_next)
    if (verbose) message(msg)
    out <- osf_mkdir(dir_root, path_next, verbose)
  }
  out
}
