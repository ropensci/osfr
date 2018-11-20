osf_tbl_node <- function(x = NULL) {
  x <- x %||% list(title = character(), id = character(), metadata = list())
  new_osf_tbl(x, class = "osf_tbl_node")
}

# expects a list, where each item is OSF entity represented as a list
as_osf_tbl_node <- function(x, ...) UseMethod("as_osf_tbl_node")

as_osf_tbl_node.default <- function(x)
  stop("No methods available to coerce this object into an osf_tbl_node")

as_osf_tbl_node.data.frame <- function(x) new_osf_tbl_node(x)

as_osf_tbl_node.list <- function(x) {
  new_osf_tbl(
    purrr::map_df(x,
                  ~ tibble::tibble(
                    title         = .x$attributes$title,
                    id            = .x$id,
                    meta          = list(.x[c("attributes", "links", "relationships")])
                  )),
    class = "osf_tbl_node"
  )
}
