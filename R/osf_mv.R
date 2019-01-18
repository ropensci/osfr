#' Copy a file or directory
#'
#' Use `osf_mv()` to move a file or directory. The `to` argument
#' determines where the copy will be made.
#'
#' @param x An [`osf_tbl_file`] containing a single file or directory.
#' @param path
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
  id <- as_id(x)

  if (!inherits(to, c("osf_tbl_node", "osf_tbl_file"))) {
    abort("Must pass an `osf_tbl_file` or `osf_tbl_node` to the `to` argument.")
  }

  if (inherits(to, "osf_tbl_file")) {
    if (is_osf_file(to)) {
      abort("If `to` is an `osf_tbl_file` it  must contain a directory, not a file.")
    }
    path <- get_meta(to, "attributes", "path")
  } else {
    # project/component path
    path <- "/"
  }

  api_url <- get_meta(x, "links", "move")
  api_path <- crul::url_parse(api_url)$path

  file_id <- strsplit(out$data$id, split = "/", fixed = TRUE)[[1]][2]
  .osf_file_retrieve(file_id)
}


.wb_file_move <- function(api_path, action, conflict) {
  action <- match.arg(action, c("move", "copy"))
  conflict <- match.arg(conflict, c("replace", "keep", "warn"))

  if (to )


  body <- list(
    action = "move",
    path = "5c410190154ce50017dc4da0/",
    conflict = ifelse(overwrite, "replace", "warn")
  )

  res <- .wb_request("post", api_path, body = body, encode = "json")
  out <- process_response(res)
  raise_error(out)
}


build_move_request <- function(x) UseMethod("build_move_request")

build_move_request.osf_tbl_file <- function(x) {
  if (is_osf_file(to)) {
    abort("If `to` is an `osf_tbl_file` it  must contain a directory, not a file.")
  }

  list(
    path = get_meta(to, "attributes", "path"),
    body = list(

    )
  )

}

build_move_request.osf_tbl_node <- function(x) {
  list(
    path = "/",
    body = list(
      resource = as_id(x)
    )
  )
}

