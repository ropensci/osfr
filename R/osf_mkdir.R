#' Create directories on OSF
#'
#' Use `osf_mkdir()` to create a new directory on OSF. Directories can be added
#' to projects, components, or nested within existing directories on OSF. If
#' `path` contains multiple directories (e.g., `"data/rawdata"`) the
#' intermediate-level directories will be created if needed.
#'
#' @param x One of the following:
#'   * An [`osf_tbl_node`] with a single OSF project or component.
#'   * An [`osf_tbl_file`] containing a single directory.
#' @param path Name of the new directory or a path ending with the new directory.
#' @template verbose
#'
#' @return An [`osf_tbl_file`] with one row containing the leaf directory
#'   specified in `path`.
#' @export
#' @examples
#' \dontrun{
#' proj <- osf_create_project("Directory Example")
#'
#' # add directory to the top-level of the Directory Example project
#' data_dir <- osf_mkdir(proj, path = "data")
#'
#' # add a subdirectory nested within data/
#' osf_mkdir(data_dir, path = "rawdata")
#'
#' # recursively create multiple directory levels within data/
#' osf_mkdir(data_dir, path = "samples/pcr/qc")
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
    msg <- sprintf("Created directory '%s/' in node %s", path_root, id)
  } else {
    msg <- sprintf("Directory '%s/' already exists in node %s", path_root, id)
  }

  if (verbose) message(msg)

  # recurse to the next-level if there is a subfolder
  path_next <- fs::path_rel(path, path_root)
  if (path_next == ".") {
    out <- dir_root
  } else {
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
    msg <- sprintf("Created sub-directory '%s/' in directory '%s/'",
                   path_root, x$name)
  } else {
    msg <- sprintf("Sub-directory '%s/' already exists in directory '%s/'",
                   path_root, x$name)
  }

  if (verbose) message(msg)

  # recurse to the next-level if there is a subfolder
  path_next <- fs::path_rel(path, path_root)
  if (path_next == ".") {
    out <- dir_root
  } else {
    out <- osf_mkdir(dir_root, path_next, verbose)
  }
  out
}
