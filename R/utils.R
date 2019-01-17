#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom purrr %>%
#' @usage lhs \%>\% rhs
NULL

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
  kind <- get_meta(x, "attributes", "kind")
  kind == "folder"
}

is_osf_file <- function(x) {
  stopifnot(inherits(x, "osf_tbl_file"))
  kind <- get_meta(x, "attributes", "kind")
  kind == "file"
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
  } else if (n == 0) {
    object <- switch(entity, row = "data frame", element = "vector")
    abort(sprintf("Can't proceed because this %s is empty.", object))
  }
  x
}


#' Convenience functions for developers to switch between accounts
#'
#' Use these functions to:
#' * switch between OSF's test and production servers
#' * switch between your development and standard PAT
#'
#' Assumes your home directory contains a `.Renviron` file that defines
#' `OSF_PAT` with your standard PAT, and your current working directory contains
#' another `.Renviron` file with the PAT you use for `test.osf.io`.
#' @noRd
NULL

osf_dev_on <- function() {
  renviron <- normalizePath(".Renviron")
  stopifnot(file.exists(renviron))
  stopifnot(readRenviron(renviron))
  Sys.setenv(OSF_SERVER = "test")
  message("osfr development mode enabled.")
  osf_auth()
}

osf_dev_off <- function() {
  renviron <- normalizePath("~/.Renviron")
  stopifnot(file.exists(renviron))
  stopifnot(readRenviron(renviron))
  Sys.unsetenv("OSF_SERVER")
  message("osfr development mode disabled.")
  osf_auth()
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
  yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", "Absolutely", "Yes, 100%")
  nos <- c("No way", "Not yet", "I don't think so", "No", "Nope", "Uhhhh... Maybe?",
           "No. That's my final answer")

  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  menu(qs[rand], title = question) == which(rand == 1)
}
