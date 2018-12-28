#' Stop execution with HTTP status code
#' @param code HTTP status code
#' @inheritParams base::stop
#' @noRd
http_error <- function(code, ...) {
  args <- list(...)
  msg <- sprintf("\n       HTTP status code %i.", code)
  stop(args, msg, call. = FALSE)
}

is_osf_url <- function(url) grepl("osf.io", tolower(url), fixed = TRUE)

is_osf_dir <- function(x) {
  stopifnot(inherits(x, "osf_tbl_file"))
  get_attr(x, "kind") == "folder"
}

is_osf_file <- function(x) {
  stopifnot(inherits(x, "osf_tbl_file"))
  get_attr(x, "kind") == "file"
}

#' Return an OSF file or folder based on name matching
#'
#' Useful in situations where we need to know if a file or directory already
#' exists on OSF and want to retrieve it without knowing its ID. This is just a
#' wrapper around `osf_ls_files` that verifies a returned entity's name exactly
#' matches the specified `name` argument. This isn't possible with
#' `osf_ls_files` since OSF API uses substring matching.
#'
#' @param name string containing the exact name to be matched against
#' @return an [`osf_tbl_file`] containing a single matching entity or zero rows
#'   if no match was found
#' @noRd
find_exact_match <- function(x, name, type = "files") {
  items <- osf_ls_files(x, pattern = name, type = type)
  items[items$name == name, ]
}


# extract OSF and Waterbutler identifiers from known URL schemes
extract_osf_id <- function(url) {
  stopifnot(is_osf_url(url))
  path <- crul::url_parse(url)$path
  purrr::map_chr(fs::path_split(path), utils::tail, 1)
}


# Transparently filter user input to single object/row
make_single <- function(x) {
  if (inherits(x, "data.frame")) {
    n <- nrow(x)
    entity <- "row"
  } else {
    n <- length(x)
    entity <- "element"
  }

  if (n > 1) {
    msg <- "This is not a vectorized function. Only the first %s of %i will be used."
    warn(sprintf(msg, entity, n))
    x <- head(x, 1)
  }
  x
}
