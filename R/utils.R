#' Identify whether logged in

logged_in <- function () {
  if (Sys.getenv('OSF_PAT' == '')) {
    return(FALSE)
  }
  return(TRUE)
}

#' Identify type of endpoint for id
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

#' Create authorization config
#'
#' @param login Boolean indicating whether login is required.
#'
#' @return httr::request configuration

get_config <- function(login) {
  if (login) {
    config <- httr::add_headers(Authorization = sprintf('Bearer %s', auth()))
  } else {
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
