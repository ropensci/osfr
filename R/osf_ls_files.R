#' List files and directories on OSF
#'
#' List the files and directories in the top-level of an OSF project, component, or
#' directory. Specify a `path` to list the contents of a particular
#' subdirectory.
#'
#' @param x one of the following:
#'   * an [`osf_tbl_node`] with a single project or component.
#'   * an [`osf_tbl_file`] with a single directory.
#' @param path list files within the specified subdirectory path
#' @template filter-type
#' @template filter-pattern
#' @template n_max
#'
#' @return an [`osf_tbl_file`] with `x`'s top-level files and directories
#' @examples
#' \dontrun{
#' # List the files and folders from https://osf.io/gep9v/
#' project <- osf_retrieve_node("gep9v")
#' files <- osf_ls_files(project)
#'
#' # List the files in the first directory
#' osf_ls_files(files[1, ])
#' }
#'
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
 .osf_list_files(x, path, type, pattern, n_max)
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
 .osf_list_files(x, path, type, pattern, n_max)
}


.osf_list_files <- function(x, path, type, pattern, n_max) {

  # manually construct path for nodes because the provided files endpoint is
  # for listing storage providers
  api_path <- switch(class(x)[1],
    osf_tbl_node = osf_path(sprintf("nodes/%s/files/osfstorage/", as_id(x))),
    osf_tbl_file = crul::url_parse(get_relation(x, "files"))$path
  )

  # recurse if path contains subdirectories
  path <- path %||% "."
  if (path != ".") {
    path_root <- fs::path_split(path)[[1]][1]
    root_dir <- find_exact_match(x, name = path_root, type = "folder")
    if (nrow(root_dir) == 0) {
      abort(sprintf("Can't find path `%s` within `%s`", path, x$name))
    }

    next_path <- fs::path_rel(path, path_root)
    res <- .osf_list_files(root_dir, next_path, type, pattern, n_max)
    return(as_osf_tbl(res, "osf_tbl_file"))
  }

  res <- .osf_paginated_request(
    method = "get",
    path = api_path,
    query = filter_files(pattern, type),
    n_max = n_max,
    verbose = FALSE
  )

  as_osf_tbl(res, subclass = "osf_tbl_file")
}
