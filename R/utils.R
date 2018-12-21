#' Stop execution with HTTP status code
#' @param code HTTP status code
#' @inheritParams base::stop
http_error <- function(code, ...) {
  args <- list(...)
  msg <- sprintf("\n       HTTP status code %i.", code)
  stop(args, msg, call. = FALSE)
}

is_osf_url <- function(url) grepl("osf.io", tolower(url), fixed = TRUE)

# extract OSF and Waterbutler identifiers from known URL schemes
extract_osf_id <- function(url) {
  stopifnot(is_osf_url(url))
  path <- crul::url_parse(url)$path
  purrr::map_chr(fs::path_split(path), tail, 1)
}

