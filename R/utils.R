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
