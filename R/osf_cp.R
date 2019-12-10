#' Copy a file or directory
#'
#' Use `osf_cp()` to make a copy of a file or directory within the same or new location.
#'
#' @param x An [`osf_tbl_file`] containing a single file or directory.
#' @param to Optional destination where the file or directory will be copied. Defaults
#'   to original location of file if not specified.
#'   This can be one of the following:
#'   * An [`osf_tbl_node`] with a single project or component.
#'   * An [`osf_tbl_file`] with a single directory.
#' @template verbose
#'
#' @return An [`osf_tbl_file`] containing the updated OSF file.
#'
#' @examples
#' \dontrun{
#' # Create an example file to upload to our example project
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
osf_cp <- function(x, to = NULL, verbose = FALSE) {
  UseMethod("osf_cp")
}

#' @export
osf_cp.osf_tbl_file <- function(x, to = NULL, verbose = FALSE) {
  x <- make_single(x)
  if (is.null(to)) to <- osf_retrieve_node(get_parent_id(x))
  out <- .wb_file_copy(
    x,
    to = to,
    action = "copy",
    verbose = verbose
  )
  as_osf_tbl(out["data"], subclass = "osf_tbl_file")
}


#' Internal method for moving/copying files
#' @noRd
#' @references
#' https://waterbutler.readthedocs.io/en/latest/api.html#actions
.wb_file_copy <- function(x, to, action, verbose) {
  action <- match.arg(action, c("move", "copy"))
  conflict <- "keep"

  if (inherits(to, "osf_tbl_file")) {
    if (is_osf_file(to)) {
      abort("If `to` is an `osf_tbl_file` it  must contain a directory, not a file.")
    }

    # verify destination is not a child of x
    is_child_dest <- fs::path_has_parent(
      get_meta(to, "attributes", "materialized_path"),
      get_meta(x, "attributes", "materialized_path")
    )
    if (is_child_dest) abort("Can't move a parent directory into its child.")
  }

  api_url <- get_meta(x, "links", "move")
  api_path <- crul::url_parse(api_url)$path

  req <- modifyList(
    .wb_file_action(to),
    list(action = action, conflict = conflict)
  )

  res <- .wb_request("post", api_path, body = req, encode = "json")
  out <- process_response(res)
  raise_error(out)

  if (verbose) message(sprintf("Copied '%s' to '%s'.", x$name, to$name))

  # retrieve osf representation of file
  file_id <- strsplit(out$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}


