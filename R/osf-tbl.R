#' OSF Tibbles
#'
#'

#' @export
osf_tbl <- function(x = NULL, subclass = NULL) {
  x <- x %||% list(name = character(), id = character(), metadata = list())
  new_osf_tbl(x, subclass)
}

#' @export
new_osf_tbl <- function(x, subclass = NULL) {
  tibble::new_tibble(x, subclass = c(subclass, "osf_tbl"))
}

#' @export
# expects a list, where each item is OSF entity represented as a list
as_osf_tbl <- function(x, subclass = NULL) UseMethod("as_osf_tbl")

#' @export
as_osf_tbl.default <- function(x, subclass = NULL)
  abort("No methods available to coerce this object into an osf_tbl")

#' @export
as_osf_tbl.data.frame <- function(x, subclass = NULL) new_osf_tbl(x, subclass)

#' @export
as_osf_tbl.list <- function(x, subclass = NULL) {

  get_name <- switch(subclass,
    osf_tbl_node = function(x) x$attributes$title,
    osf_tbl_file = function(x) x$attributes$name,
    osf_tbl_user = function(x) x$attributes$full_name
  )

  tbl <- purrr::map_df(
    x,
    ~ list(
      name = get_name(.x),
      id   = .x$id,
      meta = list(.x[c("attributes", "links", "relationships")])
    )
  )

  new_osf_tbl(tbl, subclass)
}

#' @export
`[.osf_tbl` <- function(x, i, j, drop = FALSE) {
  structure(NextMethod(), class = class(x))
}
