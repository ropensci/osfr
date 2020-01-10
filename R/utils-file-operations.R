#' Internal method for moving/copying files
#' @noRd
#' @references
#' https://waterbutler.readthedocs.io/en/latest/api.html#actions
.wb_file_action <- function(x, to, action, overwrite, verbose) {
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

  if (verbose & action == "move") message(sprintf("Moved '%s' to '%s'.", x$name, to$name))
  if (verbose & action == "copy") message(sprintf("Copied '%s' to '%s'.", x$name, to$name))

  wb2osf(out)
}


# Construct the move/copy request's body
build_move_request <- function(x) {
  switch(class(x)[1],

    osf_tbl_node =   list(
      path = "/",
      resource = unclass(as_id(x)),
      provider = "osfstorage"
    ),

    osf_tbl_file = list(
      path = get_meta(x, "attributes", "path")
    )
  )
}
