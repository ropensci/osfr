#' Process a response from the OSF API
#'
#' @param res
#'
#' @return list
#'
#' @importFrom jsonlite fromJSON
process_response <- function(res) {
  stopifnot(class(res)[1] == "HttpResponse")
  out <- jsonlite::fromJSON(res$parse("UTF-8"), simplifyVector = FALSE)

  if (is.null(out$data)) {
    return(out)

  # Process accordingly if response includes multiple entities or single entity
  } else if(rlang::is_named(out$data)) {
    out$data <- parse_datetime_attrs(out$data)
  } else {
    out$data <- purrr::map(out$data, parse_datetime_attrs)
  }
  out
}

# Convert datetime attributes to POSIXct objects
# param x an OSF response list that contains an attributes element
#' @importFrom purrr modify_at
parse_datetime_attrs <- function(x) {
  stopifnot(is.list(x))
  stopifnot("attributes" %in% names(x))

  x$attributes <-  purrr::modify_at(x$attributes,
      .at = c("date_registered", "date_created", "date_modified"),
      .f = parse_datetime
  )

  return(x)
}

# Some OSF responses contain empty date fields e.g., creating new folders
# this wrapper around as.POSIXct will pass through NULLs instead of erroring
parse_datetime <- function(x) {
  if(is.null(x)) return(x)
  as.POSIXct(x, format = "%Y-%m-%dT%X", tz = "UTC")
}

