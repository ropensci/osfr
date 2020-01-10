#' Copy a file or directory
#'
#' Use `osf_cp()` to make a copy of a file or directory in a new location.
#'
#' @inherit osf_mv params return details
#' @param to Destination where the file or directory will be copied.
#'   This can be one of the following:
#'   * An [`osf_tbl_node`] with a single project or component.
#'   * An [`osf_tbl_file`] with a single directory.
#'
#' @family OSF file operations
#' @examples
#' \dontrun{
# Create an example file to upload to our example project
#' project <- osf_create_project("Flower Data")
#'
#' write.csv(iris, file = "iris.csv")
#' data_file <- osf_upload(project,"iris.csv")
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
osf_cp <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  UseMethod("osf_cp")
}

#' @export
osf_cp.osf_tbl_file <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  .wb_file_action(
    make_single(x),
    to = to,
    action = "copy",
    overwrite = overwrite,
    verbose = verbose
  )
}
