#' List files and folders
#'
#' List the files and folders in the top-level of an OSF project, component, or
#' directory. Specify a `path` to list the contents of a particular
#' subdirectory.
#'
#' @param x an [`osf_tbl_node`] representing an OSF project or component or an
#'   [`osf_tbl_file`] containing a directory
#' @param path list files within the specified subdirectory path
#' @param type filter query to include only `"files"` or
#'   `"folders"`
#' @param pattern filter query for entities whose name contains the
#'   specified `pattern`
#' @template n_max
#'
#' @return an [`osf_tbl_file`] with `x`'s top-level files and directories
#' @export
osf_ls_files <-
  function(x,
           path = NULL,
           type = "any",
           pattern = NULL,
           n_max = 10) {
    UseMethod("osf_ls_files")
}

#' @export
osf_ls_files.osf_tbl_node <-
  function(x,
           path = NULL,
           type = "any",
           pattern = NULL,
           n_max = 10) {

  x <- make_single(x)
  api_path <- osf_path(sprintf("nodes/%s/files/osfstorage/", as_id(x)))

  path <- path %||% "."
  if (path == ".") {
    out <- .osf_list_files(api_path, type, pattern, n_max)
    items <- as_osf_tbl(out, subclass = "osf_tbl_file")
    return(items)
  }

  # look for specified path within the parent node
  path_root <- fs::path_split(path)[[1]][1]
  osf_dir <- .osf_dir_exists(api_path, path_root, n_max)

  # list files within the next subdirectory
  next_path <- fs::path_rel(path, path_root)
  out <- osf_ls_files(osf_dir, next_path, type, pattern, n_max)
  out
}

#' @export
osf_ls_files.osf_tbl_file <-
  function(x,
           path = NULL,
           type = "any",
           pattern = NULL,
           n_max = 10) {
  x <- make_single(x)

  if (is_osf_file(x)) {
    abort("Listing an `osf_tbl_file` requires a directory\n* `x` contains a file")
  }

  # list files using extracted API endpoint
  url <- get_relation(x, "files")
  api_path <- crul::url_parse(url)$path

  path <- path %||% "."
  if (path == ".") {
    out <- .osf_list_files(api_path, type, pattern, n_max)
    items <- as_osf_tbl(out, subclass = "osf_tbl_file")
    return(items)
  }

  # look for specified path within the root directory
  path_root <- fs::path_split(path)[[1]][1]
  osf_dir <- .osf_dir_exists(api_path, path_root, n_max)

  # recurse if path contains subdirectories
  next_path <- fs::path_rel(path, path_root)
  out <- osf_ls_files(osf_dir, next_path, type, pattern, n_max)
  out
  }


# error if the specified directory doesn't exist or return the osf_tbl_file
.osf_dir_exists <- function(path, dirname, n_max) {
  out <- .osf_list_files(path, type = "folder", pattern = dirname, n_max)
  items <- as_osf_tbl(out, subclass = "osf_tbl_file")
  osf_dir <- items[items$name == dirname, ]

  if (nrow(osf_dir) == 0) {
    abort(sprintf("Can't find path `%s`. Are you sure it exists?", dirname))
  }
  return(osf_dir)
}
