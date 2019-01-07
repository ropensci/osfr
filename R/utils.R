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
  Sys.setenv(OSF_USE_SERVER = "test")
  message("osfr development mode enabled.")
  osf_auth()
}

osf_dev_off <- function() {
  renviron <- normalizePath("~/.Renviron")
  stopifnot(file.exists(renviron))
  stopifnot(readRenviron(renviron))
  Sys.unsetenv("OSF_USE_SERVER")
  message("osfr development mode disabled.")
  osf_auth()
}


#' Include an interactive menu to confirm action
#'
#' Use \code{yesno_menu} to confirm an otherwise permanent change to an OSF node.
#' Typically embedded within a function, a question specific to the function
#' task is prespecified for the title argument of \code{menu()}.
#'
#' @param question A \code{character} specific to the action needing verification.
#' @param node_id A \code{character} indicating OSF node under consideration.
#' @return A \code{logical} verifying intent.
#' @examples
#' question <- "Are you sure you want to permanently change node"
#' node <- "z756a"
#' yesno_menu(question, node)
#' @importFrom utils menu
#' @noRd


yesno_menu <- function(question, id) {
  yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely", "Yes, 100%")
  nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?", "No. That's my final answer")

  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  menu(qs[rand], title= sprintf("%s, %s ?", question, id)) == which(rand == 1)
}

