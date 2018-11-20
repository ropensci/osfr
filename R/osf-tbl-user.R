osf_tbl_user <- function(x = NULL) {
  x <- x %||% list(name = character(), id = character(), metadata = list())
  new_osf_tbl(x, class = "osf_tbl_user")
}

# expects a list, where each item is OSF entity represented as a list
as_osf_tbl_user <- function(x, ...) UseMethod("as_osf_tbl_user")

as_osf_tbl_user.default <- function(x)
  stop("No methods available to coerce this object into an osf_tbl_user")

as_osf_tbl_user.data.frame <- function(x) new_osf_tbl_user(x)

as_osf_tbl_user.list <- function(x) {
  new_osf_tbl(
    purrr::map_df(x,
                  ~ tibble::tibble(
                    name          = .x$attributes$full_name,
                    id            = .x$id,
                    meta          = list(.x[c("attributes", "links", "relationships")])
                  )),
    class = "osf_tbl_user"
  )
}
