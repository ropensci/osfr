#' Copy a file or directory
#'
#' Use `osf_cp()` to make a copy of a file or directory within the same or new location.
#'
#' @param x An [`osf_tbl_file`] containing a single file or directory.
#' @param to Optional destination where the file or directory will be copied. Defaults
#'   to duplicate file in original location if unspecified.
#'   This can be one of the following:
#'   * An [`osf_tbl_node`] with a single project or component.
#'   * An [`osf_tbl_file`] with a single directory.
#' @param overwrite Logical, if a file or directory with the same name already
#'   exists at the destination should it be replaced with `x`?
#' @template verbose
#'
#' @return An [`osf_tbl_file`] containing the updated OSF file.
#'
#' @examples
#' \dontrun{
# Create an example file to upload to our example project
#' project <- osf_create_project("Flower Data")
#' write.csv(iris, file = "iris.csv")
#' data_file <- osf_upload(project,"iris.csv")
#'
#' # Make a copy of the file in the same location. A suffix is added to avoid name conflicts
#' data_file <- osf_cp(data_file)
#'
#' # Create a new directory to copy our file to
#' data_dir <- osf_mkdir(project, "data")
#'
#' # Copy the file to our data directory
#' data_file <- osf_cp(data_file, to = data_dir)
#'
#' # Copy directory to new component
#' data_comp <- osf_create_component(project, title = "data", category = "data")
#' data_dir %>%
#'   osf_cp(to = data_comp) %>%
#'   osf_open()
#' }
#'
#' @export
osf_cp <- function(x, to = NULL, overwrite = FALSE, verbose = FALSE) {
  UseMethod("osf_cp")
}

#' @export
osf_cp.osf_tbl_file <- function(x, to = NULL, overwrite = FALSE, verbose = FALSE) {
  x <- make_single(x)
  if (is.null(to)) to <- osf_retrieve_node(get_parent_id(x))
  out <- .wb_file_action(
    x,
    to = to,
    action = "copy",
    overwrite = overwrite,
    verbose = verbose
  )
}
