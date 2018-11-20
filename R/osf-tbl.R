osf_tbl <- function(x = NULL) {
  x <- x %||% list(id = character(), metadata = list())
  new_osf_tbl(x)
}

new_osf_tbl <- function(x, class = NULL) {
  tibble::new_tibble(x, subclass = c(class, "osf_tbl"))
}

# expects a list, where each item is OSF entity represented as a list
as_osf_tbl <- function(x, ...) UseMethod("as_osf_tbl")

as_osf_tbl.default <- function(x)
  stop("No methods available to coerce this object into an osf_tbl")

as_osf_tbl.data.frame <- function(x) new_osf_tbl(x)

as_osf_tbl.list <- function(x) {
  new_osf_tbl(
    purrr::map_df(x,
                  ~ list(
                    id            = .x$id,
                    meta          = list(.x[c("attributes", "links", "relationships")])
                  )
    )
  )
}

#' @export
`[.osf_tbl` <- function(x, i, j, drop = FALSE) {
  structure(NextMethod(), class = class(x))
}
