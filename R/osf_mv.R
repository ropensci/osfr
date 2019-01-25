#' Move a file or directory
#'
#' Use `osf_mv()` to move a file or directory to a new project, component, or
#' subdirectory.
#'
#' @param x An [`osf_tbl_file`] containing a single file or directory.
#' @param to The destination where the file or directory will be copied to. This
#'   can be one of the following:
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
#' data_file %>%
#'   osf_mv(to = data_comp) %>%
#'   osf_open()
#' }
#'
#' @export
osf_mv <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  UseMethod("osf_mv")
}

#' @export
osf_mv.osf_tbl_file <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  x <- make_single(x)
  out <- .wb_file_move(
    x,
    to = to,
    action = "move",
    overwrite = overwrite,
    verbose = verbose
  )
  as_osf_tbl(out["data"], subclass = "osf_tbl_file")
}


#' Internal method for moving/copying files
#' @noRd
#' @references
#' https://waterbutler.readthedocs.io/en/latest/api.html#actions
.wb_file_move <- function(x, to, action, overwrite, verbose) {
  action <- match.arg(action, c("move", "copy"))
  conflict <- ifelse(overwrite, "replace", "warn")

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
    build_move_request(to),
    list(action = action, conflict = conflict)
  )

  res <- .wb_request("post", api_path, body = req, encode = "json")
  out <- process_response(res)
  raise_error(out)

  if (verbose) message(sprintf("Moved '%s' to '%s'.", x$name, to$name))

  # retrieve osf representation of file
  file_id <- strsplit(out$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}


# Construct the move/copy request's body
build_move_request <- function(x) UseMethod("build_move_request")

build_move_request.osf_tbl_file <- function(x) {
  list(
    path = get_meta(x, "attributes", "path")
  )
}

build_move_request.osf_tbl_node <- function(x) {
  list(
    path = "/",
    resource = unclass(as_id(x)),
    provider = "osfstorage"
  )
}
