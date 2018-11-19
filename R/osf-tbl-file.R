osf_tbl_file <- function(x = NULL) {
  x <- x %||% list(name = character(), id = character(), metadata = list())
  new_osf_tbl(x, class = "osf_tbl_file")
}


# expects a list, where each item is OSF entity represented as a list
as_osf_tbl_file <- function(x, ...) UseMethod("as_osf_tbl_file")

as_osf_tbl_file.default <- function(x)
  stop("No methods available to coerce this object into an osf_tbl_file")

as_osf_tbl_file.data.frame <- function(x) new_osf_tbl_file(x)

as_osf_tbl_file.list <- function(x) {
  new_osf_tbl_file(
      purrr::map_df(x,
      ~ tibble::tibble(
        name          = .x$attributes$name,
        id            = .x$id,
        # path          = .x$attributes$materialized_path,
        # kind          = .x$attributes$kind,
        meta          = list(.x[c("attributes", "links", "relationships")])
      )
    )
  )
}
