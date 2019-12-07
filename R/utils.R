#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom purrr %>%
#' @usage lhs \%>\% rhs
NULL

#' Shortcut for constructing a URL path from components
#' Also removes double forward slashes.
#' @param ... character vectors with path components
#' @noRd
url_path <- function(...) {
  gsub("\\/{2,}", "/", paste0(list(...), collapse = "/"))
}

#' Selectively prepends major API version to path when necessary A hacky fix but
#' we need it to account for requests based on URLs extracted from API response
#' links (rather than using the endpoint functions), which already include the
#' API version.
#' @param path A URL path
#' @param version Positive number indicating the specific API version
#' @noRd
prepend_version <- function(path, version) {
  stopifnot(is.character(path))
  stopifnot(is.numeric(version))
  path <- sub("^\\/?v\\d\\/", "", path)
  url_path(paste0("v", floor(version)), path)
}

#' Stop execution with HTTP status code
#' @param code HTTP status code
#' @inheritParams base::stop
#' @noRd
http_error <- function(code, ...) {
  args <- list(...)
  msg <- sprintf("\n       HTTP status code %i.", code)
  stop(args, msg, call. = FALSE)
}

#' Inform user the API request failed and will be retried
#' @param res response object from the failed request
#' @param time wait time in seconds
#' @noRd
retry_message <- function(res, time) {
  msg <- sprintf(
    "Request failed (Status code: %s). Retrying in %ds...",
    res$status_code, ceiling(time)
  )
  if (!is.null(getOption("osfr.log"))) logger::log_info(msg)
  message(msg)
}

is_osf_url <- function(url) grepl("osf.io", tolower(url), fixed = TRUE)

is_osf_dir <- function(x) {
  stopifnot(inherits(x, "osf_tbl_file"))
  kind <- get_meta(x, "attributes", "kind")
  kind == "folder"
}

is_osf_file <- function(x) {
  stopifnot(inherits(x, "osf_tbl_file"))
  kind <- get_meta(x, "attributes", "kind")
  kind == "file"
}


# extract OSF and Waterbutler identifiers from known URL schemes
#' @importFrom fs path_split
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
  } else if (n == 0) {
    object <- switch(entity, row = "data frame", element = "vector")
    abort(sprintf("Can't proceed because this %s is empty.", object))
  }
  x
}

#' Wrapper around base::Map that rbinds the results
#'
#' I like base::Map because it returns a named list when mapping over a
#' character vector.
#' @noRd
map_rbind <- function(f, ...) {
  out <- base::Map(f, ...)
  do.call("rbind", out)
}

#' Include an interactive menu to confirm action
#'
#' Use \code{yesno_menu} to confirm an otherwise permanent change to an OSF node.
#' Typically embedded within a function, a question specific to the function
#' task is prespecified for the title argument of \code{menu()}.
#'
#' @param question A character describing the action needing verification.
#' @return A `logical` verifying intent.
#' @examples
#' question <- "Are you sure you want to permanently delete?"
#' yesno_menu(question)
#' @importFrom utils menu
#' @noRd

yesno_menu <- function(question) {
  yeses <- c(
    "Yes",
    "Definitely",
    "For sure",
    "Yup",
    "Yeah",
    "Absolutely",
    "Yes, 100%"
  )

  nos <- c(
    "No way",
    "Not yet",
    "I don't think so",
    "No",
    "Nope",
    "Uhhhh... Maybe?",
    "No. That's my final answer"
  )

  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  menu(qs[rand], title = question) == which(rand == 1)
}


#' Clean local directory path
#'
#' Returns an absolute path when `x` is above the current working directory,
#' or a relative path when `x` is nested within the current working directory.
#' Path expansion is also performed.
#' @param x A local directory path
#' @noRd
clean_local_path <- function(x) {
  if (fs::path_has_parent(x, getwd())) {
    return(fs::path_rel(x))
  } else {
    return(x)
  }
}


#' Clean OSF directory path
#'
#' This removes reserved filenames (ie, `..`, and platform-specific file
#' separators) from the begining of file paths in order to create valid directory
#' names for OSF.
#'
#' @param x A local directory path
#' @importFrom fs path_join
#' @noRd
clean_osf_path <- function(x) {
  stopifnot(rlang::is_scalar_character(x))

  reserved <- c(".", "..", .Platform$file.sep)
  parts <- fs::path_split(x)[[1]]

  # recursively remove offending parts from the begining of a path
  while (parts[1] %in% reserved) parts <- parts[-1]
  if (rlang::is_empty(parts)) parts <- "."

  fs::path_join(parts)
}
