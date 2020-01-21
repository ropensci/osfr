#' Process OSF API responses
#'
#' Convert HttpResponse objects to R lists.
#'
#' JSON objects returned by succesful requests are converted to lists and
#' undergo some additional processing. Currently this only includes converting
#' embedded dates to POSIXct objects.
#'
#' When a request fails the OSF API will return a JSON object with a key
#' \code{errors} containing error messages and additional information about the
#' failure. These are also converted to lists and returned without erroring so
#' that the parent functions can determine how to proceed.
#'
#' @param res HTTP response from the OSF or Waterbutler API
#'
#' @return list
#'
#' @noRd
#' @importFrom jsonlite fromJSON
process_response <- function(res) {
  stopifnot(class(res)[1] == "HttpResponse")

  if (res$status_code > 500) {
    abort(paste0(
      "Encountered an unexpected error with the OSF API\n",
      "Please report this at https://github.com/ropensci/osfr/issues\n",
      "* Status code: ", res$status_code, "\n",
      "* Request: ", res$request$url$url, "\n",
      "* Resonse: \n", res$parse("UTF-8")
    ))
  }

  out <- jsonlite::fromJSON(res$parse("UTF-8"), simplifyVector = FALSE)
  domain <- crul::url_parse(res$url)$domain

  if (grepl("files", domain)) {
    out <- .process_wb_response(out)
  } else {
    out <- .process_osf_response(out)
  }

  if (!is.null(out$errors)) out$status_code <- res$status_code
  out
}

.process_osf_response <- function(x) {
  if (is.null(x$errors)) {
    # process dates in successful responses
    x["data"] <- purrr::modify_depth(
      x["data"],
      .f = parse_datetime_attrs,
      .depth = ifelse(is_entity_collection(x), 2, 1)
    )
  }

  x
}

.process_wb_response <- function(x) {

  # waterbutler errors contain a key code with the http status code and a key
  # message with the error message
  if (is.null(x$code)) {
    # waterbutler returns modified and modified_utc, to be consistent with osf
    # responses we store the `modified_utc` value in the `modified`
    x$data$attributes$modified <- parse_datetime(x$data$attributes$modified_utc)
    x$data$attributes$modified_utc <- NULL

  } else {
    # reformat waterbutler's error response to be consistent with OSF's
    x <- list(
      errors = list(list(detail = x$message))
    )
  }
  x
}

#' Stop and report API errors
#' @param x list returned by \code{process_response()}
#' @noRd
raise_error <- function(x) {
  if (!is.null(x$errors)) http_error(x$status_code, x$errors[[1]]$detail)
}

# Convert datetime attributes to POSIXct objects
# param x an OSF response list that contains an attributes element
parse_datetime_attrs <- function(x) {
  stopifnot(is.list(x))
  stopifnot("attributes" %in% names(x))

  dt_keys <- intersect(
    c("date_registered", "date_created", "date_modified", "modified_utc"),
    names(x$attributes)
  )

  dt_vals <- lapply(x$attributes[dt_keys], parse_datetime)

  x$attributes <- modifyList(x$attributes, dt_vals)
  return(x)
}

# Some OSF responses contain empty date fields e.g., creating new folders
# this wrapper around as.POSIXct will pass through NULLs instead of erroring
#
# Staring with API v2.2 all date fields are standardized to UTC
parse_datetime <- function(x) {
  if (is.null(x)) return(x)
  as.POSIXct(x, format = "%Y-%m-%dT%X", tz = "UTC")
}

# https://developer.osf.io/#tag/Entities-and-Entity-Collections
is_entity_collection <- function(x) is.null(names(x$data))
