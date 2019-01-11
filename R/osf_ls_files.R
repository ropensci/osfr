#' List files and directories on OSF
#'
#' List the files and directories in the top-level of an OSF project, component,
#' or directory. Specify a `path` to list the contents of a particular
#' subdirectory.
#'
#' @param x One of the following:
#'   * An [`osf_tbl_node`] with a single project or component.
#'   * An [`osf_tbl_file`] with a single directory.
#' @param path List files within the specified subdirectory path.
#' @template filter-type
#' @template filter-pattern
#' @template n_max
#' @template verbose
#'
#' @return An [`osf_tbl_file`] with one row for each file or directory.
#' @examples
#' \dontrun{
#' # Retrieve the Psychology Reproducibility Project from OSF
#' pysch_rp <- osf_retrieve_node("ezum7")
#'
#' # List all files and directories
#' osf_ls_files(pysch_rp)
#'
#' # ...only the directories
#' osf_ls_files(pysch_rp, type = "folder")
#'
#' # ...only PDF files
#' osf_ls_files(pysch_rp, type = "file", pattern = "pdf")
#'
#' # List the contents of the first directory
#' osf_ls_files(pysch_rp, path = "RPP_SI_Figures")
#' }
#' @seealso [`osf_ls_nodes()`] to generate a list of projects and components.
#' @export
osf_ls_files <-
  function(x,
           path = NULL,
           type = "any",
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {
    UseMethod("osf_ls_files")
}

#' @export
osf_ls_files.osf_tbl_node <-
  function(x,
           path = NULL,
           type = "any",
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {

  x <- make_single(x)
 .osf_list_files(x, path, type, pattern, n_max, verbose)
}

#' @export
osf_ls_files.osf_tbl_file <-
  function(x,
           path = NULL,
           type = "any",
           pattern = NULL,
           n_max = 10,
           verbose = FALSE) {

  x <- make_single(x)
  if (is_osf_file(x)) {
    abort("Listing an `osf_tbl_file` requires a directory\n* `x` contains a file")
  }
 .osf_list_files(x, path, type, pattern, n_max, verbose)
}


.osf_list_files <- function(x, path, type, pattern, n_max, verbose) {

  # manually construct path for nodes because the provided files endpoint is
  # for listing storage providers
  api_path <- switch(class(x)[1],
    osf_tbl_node = .osf_api_path(sprintf("nodes/%s/files/osfstorage/", as_id(x))),
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
    res <- .osf_list_files(root_dir, next_path, type, pattern, n_max, verbose)
    return(as_osf_tbl(res, "osf_tbl_file"))
  }

  res <- .osf_paginated_request(
    method = "get",
    path = api_path,
    query = filter_files(pattern, type),
    n_max = n_max,
    verbose = verbose
  )

  as_osf_tbl(res, subclass = "osf_tbl_file")
}
