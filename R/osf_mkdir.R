#' Create directories on OSF
#'
#' Use `osf_mkdir()` to add new directories to projects, components, or nested
#' within existing OSF directories. If `path` contains multiple directory
#' levels (e.g., `"data/rawdata"`) the intermediate-level directories are
#' created automatically. If the directory you're attempting to create already
#' exists on OSF it will be silently ignored and included in the output.
#'
#' @param x One of the following:
#'   * An [`osf_tbl_node`] with a single OSF project or component.
#'   * An [`osf_tbl_file`] containing a single directory.
#' @param path Name of the new directory or a path ending with the new directory.
#' @template verbose
#'
#' @return An [`osf_tbl_file`] with one row containing the leaf directory
#'   specified in `path`.
#'
#' @family OSF file operations
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
#' @export

osf_mkdir <- function(x, path, verbose = FALSE) {
  UseMethod("osf_mkdir")
}

#' @export
osf_mkdir.osf_tbl_node <- function(x, path, verbose = FALSE) {
  x <- make_single(x)
  recurse_path(x, path, missing_action = "create", verbose)
}

#' @export
osf_mkdir.osf_tbl_file <- function(x, path, verbose = FALSE) {
  if (is_osf_file(x)) abort("Can't create directories within a file.")
  x <- make_single(x)
  recurse_path(x, path, missing_action = "create", verbose)
}


#' Create a single folder on OSF
#'
#' This wraps the create folder endpoint on Waterbutler but retrieves the newly
#' created directory from OSF because Waterbutler returns only a subset of the
#' information provided by OSF.
#'
#' @param id GUID for an OSF project or component
#' @param name Name of the new directory (note: this must be the name of a
#'   single directory, not a path)
#' @param fid Optional, provide a Waterbutler folder ID to create the new folder
#'   within the specified existing folder.
#' @noRd

.osf_mkdir <- function(id, name, fid = NULL) {
  res <- .wb_create_folder(id, name, fid)
  raise_error(res)
  dir_id <- gsub("/", "", res$data$attributes$path, fixed = TRUE)
  osf_retrieve_file(dir_id)
}


#' Recurse a directory path
#'
#' Given a path like 'root/subdir1/subdir2', this will retrieve each directory
#' level from OSF and return the leaf directory. The `missing_action` argument
#' determines what happens if an intermediate directory does not exist.
#'
#' @param x An `osf_tbl_node` or an `osf_tbl_file` with a directory.
#' @param path A scalar vector containing a path of directories.
#' @param missing_action Either `"error"` or `"create"` to create the missing
#'   directory.
#' @importFrom fs path_rel
#' @noRd
recurse_path <- function(x, path, missing_action = "error", verbose = FALSE) {
  missing_action <- match.arg(missing_action, c("error", "create"))

  stopifnot(rlang::is_scalar_character(path))

  path_dirs <- fs::path_split(clean_osf_path(path))[[1]]
  path_root <- path_dirs[1]

  # return the original destination
  if (path_root == ".") return(x)

  # ensure the retrieved directory and path_root have the same name
  root_dir <- osf_find_file(x, type = "folder", pattern = path_root)

  if (nrow(root_dir) == 0) {
    if (missing_action == "error") {
      abort(sprintf("Can't find directory '%s' in `%s`", path_root, x$name))
    }

    # create the missing directory
    if (inherits(x, "osf_tbl_node")) {
      root_dir <- .osf_mkdir(as_id(x), name = path_root)
      msg <- sprintf("Created directory '%s/' in node %s",
                     path_root, as_id(x))
    } else {
      root_dir <- .osf_mkdir(get_parent_id(x), name = path_root, fid = as_id(x))
      msg <- sprintf("Created sub-directory '%s/' in directory '%s/'",
                   path_root, x$name)
    }

  } else {
    msg <- sprintf("Navigating to sub-directory '%s/' in '%s'",
                   path_root, x$name)
  }

  if (verbose) message(msg)

  # recurse to the next-level if there is a subfolder
  path_next <- fs::path_rel(path, path_root)
  if (path_next == ".") {
    out <- root_dir
  } else {
    out <- Recall(root_dir, path_next, missing_action, verbose)
  }
  out
}
