#' Copy a file or directory
#'
#' Use `osf_mv()` to move a file or directory. The `to` argument
#' determines where the copy will be made.
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
#' @return Invisibly returns `TRUE` if deletion was successful.
#'
#'
#' @export
osf_mv <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  UseMethod("osf_mv")
}

#' @export
osf_mv.osf_tbl_file <- function(x, to, overwrite = FALSE, verbose = FALSE) {
  x <- make_single(x)
  out <- .wb_file_move(x, to, action = "move", overwrite = overwrite)
  as_osf_tbl(out["data"], subclass = "osf_tbl_file")
}


#' Internal method for moving/copying files
#' @noRd
#' @references
#' https://waterbutler.readthedocs.io/en/latest/api.html#actions
.wb_file_move <- function(x, to, action, overwrite) {
  action <- match.arg(action, c("move", "copy"))
  conflict <- ifelse(overwrite, "replace", "warn")

  api_url <- get_meta(x, "links", "move")
  api_path <- crul::url_parse(api_url)$path

  req <- modifyList(
    build_move_request(to),
    list(action = action, conflict = conflict)
  )

  res <- .wb_request("post", api_path, body = req, encode = "json")
  out <- process_response(res)
  raise_error(out)

  # retrieve osf representation of file
  file_id <- strsplit(out$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}


# Construct the move/copy request's body
build_move_request <- function(x) UseMethod("build_move_request")

build_move_request.osf_tbl_file <- function(x) {
  if (is_osf_file(x)) {
    abort("If `to` is an `osf_tbl_file` it  must contain a directory, not a file.")
  }
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

