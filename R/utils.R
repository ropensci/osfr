#' Identify whether user is logged in
#'
#' This function identifies whether the user has logged in by providing a
#' personal access token (PAT).
#'
#' @return Boolean indicating logged in status

logged_in <- function () {
  if (Sys.getenv('OSF_PAT') == '') {
    return(FALSE)
  }
  return(TRUE)
}

#' Identify type of endpoint for id
#'
#' This function takes an OSF id and returns the type of endpoint for that id.
#' Only returns types for the nodes and files endpoints.
#'
#' @param id OSF id to check (osf.io/xxxxx).
#'
#' @return Endpoint of id as character (nodes | files)

process_type <- function(id) {
  config <- get_config(logged_in())
  call_nodes <- httr::GET(construct_link(sprintf('nodes/%s', id)), config)
  call_files <- httr::GET(construct_link(sprintf('files/%s', id)), config)

  if (!call_nodes$status_code == 200 && !call_files$status_code) {
    stop('Failed. Are you sure you have access to the id or that it is valid?')
  } else if (call_nodes$status_code == 200) {
    res <- process_json(call_nodes)
  } else {
    res <- process_json(call_files)
  }

  if (is.null(res$data$type)) {
    return('')
  }

  return(res$data$type)
}

#' Function to parse API call
#'
#' @param x Object containing the result of an API call.
#'
#' @return Parsed JSON object in the form of an R object.

process_json <- function(x) {
  rjson::fromJSON(httr::content(x, 'text', encoding = "UTF-8"))
}

#' Processing whether a category is valid
#'
#' This functions throws an error if the provided category is not a valid
#' category for OSF.
#'
#' @param category Category to check for validity. Valid categories:
#' \itemize{
#'   \item project
#'   \item hypothesis
#'   \item methods and measures
#'   \item procedure
#'   \item instrumentation
#'   \item data
#'   \item analysis
#'   \item communication
#'   \item other
#' }

process_category <- function(category = '') {
  if (!category %in% c('project',
                      'hypothesis',
                      'methods and measures',
                      'procedure',
                      'instrumentation',
                      'data',
                      'analysis',
                      'communication',
                      'other')) {
    stop('Please input proper category, see documentation')
  }
}

#' Process Pagination
#'
#' Processes the paginated data returned by the OSF API and returns a list with
#' all of the pages combined.
#'
#' @param res The initial list returned from the OSF API and run through
#' `osfr::process_json()`. Must contain the links section.
#' @param config The configuration used in the initial call to the OSF API.
#'
#' @return List of all of the pages from the API (including the input list).

process_pagination <- function(res, config) {
  # Create variable to hold original page
  combined_list <- res$data

  # Use the first page of the returned data to get the next page link
  next_page_link <- res$links$`next`

  # While next page link is not null, run loop
  while(!is.null(next_page_link)) {

    # Call down the next page
    new_page <- process_json(httr::GET(next_page_link, config))

    # Save new page next page link to the next page variable
    next_page_link <- new_page$links$`next`

    # Combine current pages and new page
    combined_list <- c(combined_list, new_page$data)
  }

  # Return combined data
  return(combined_list)
}

#' Create authorization config
#'
#' @param login Boolean indicating whether login is required.
#'
#' @return httr::request configuration

get_config <- function(login) {
  if (!logged_in()) {
    stop('Not logged in.')
  } else if (login) {
    config <- httr::add_headers(Authorization = sprintf('Bearer %s', auth()))
  } else  {
    config <- list()
  }

  return(config)
}

#' Check OSF id
#'
#' @param id OSF id (osf.io/xxxx; just xxxx)
#'
#' @return Boolean

is_valid_osf_id <- function(id) {
  grepl('\\w{5}', id)
}

pre_slash <- function(x) {
  if (!substr(x, 1, 1) == '/')
    x <- paste0('/', x)
  x
}

#' Function to correct link into something parsable
#'
#' @param x String to correct
#'
#' @return Parsable link

rm_space <- function(x) {
  res <- gsub(x, pattern = "\\s", replacement = "%20", perl = TRUE)

  return(res)
}

#' Stop execution with HTTP status code
#' @param code HTTP status code
#' @inheritParams base::stop
http_error <- function(code, ...) {
  args <- list(...)
  msg <- sprintf("\n       HTTP status code %i.", code)
  stop(args, msg, call. = FALSE)
}
