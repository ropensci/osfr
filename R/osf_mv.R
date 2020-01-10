#' Move a file or directory
#'
#' Use `osf_mv()` to move a file or directory to a new project, component, or
#' subdirectory.
#'
#' Note that a file (or directory) cannot be moved or copied onto itself, even
#' if `overwrite = TRUE`.
#'
#' @param x An [`osf_tbl_file`] containing a single file or directory.
#' @param to Destination where the file or directory will be moved. This
#'   can be one of the following:
#'   * An [`osf_tbl_node`] with a single project or component.
#'   * An [`osf_tbl_file`] with a single directory.
#' @param overwrite Logical, if a file or directory with the same name already
#'   exists at the destination should it be replaced with `x`?
#' @template verbose
#'
#' @return An [`osf_tbl_file`] containing the updated OSF file.
#'
#' @family OSF file operations
#' @examples
#' \dontrun{
#' # Create an example file to upload to our example project
#' project <- osf_create_project("Flower Data")
#'
#' write.csv(iris, file = "iris.csv")
#' data_file <- osf_upload(project,"iris.csv")
#'
#' # Create a new directory to move our file to
#' data_dir <- osf_mkdir(project, "data")
#'
#' # Move the file to our data directory
#' data_file <- osf_mv(data_file, to = data_dir)
#'
#' # Move our data directory to a new component
#' data_comp <- osf_create_component(project, title = "data", category = "data")
#' data_dir %>%
#'   osf_mv(to = data_comp) %>%
#'   osf_open()
#' }
#'
#' @export
#' @importFrom fs path_has_parent

osf_mv <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  UseMethod("osf_mv")
}

#' @export
osf_mv.osf_tbl_file <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  x <- make_single(x)
  .wb_file_action(
    x,
    to = to,
    action = "move",
    overwrite = overwrite,
    verbose = verbose
  )
}
