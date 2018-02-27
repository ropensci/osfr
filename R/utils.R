#' Construct an API link with proper base
#'
#' @param request The request link to be combined with the base API link.
#'
#' @return The full request link with proper base
#' @examples
#' \dontrun{
#' construct_link('nodes/{node_id}/files/')
#' }

construct_link <- function(request = NULL) {

  if (Sys.getenv('OSF_USE_TEST_SERVER') == 'test') {
    base <- 'https://test-api.osf.io/v2/'
  } else if (Sys.getenv('OSF_USE_TEST_SERVER') == 'staging') {
    base <- 'https://staging-api.osf.io/v2/'
  } else {
    base <- 'https://api.osf.io/v2/'
  }

  return(paste0(base, request))
}

#' Construct a waterbutler API link with proper base
#'
#' @param id OSF id
#' @param provider Storage provider (osfstorage, github, etc)
#' @param request Request for waterbutler
#'
#' @return Waterbutler link

construct_link_files <- function(id = NULL, provider = 'osfstorage', request = NULL) {
  if (Sys.getenv('OSF_USE_TEST_SERVER') == 'test') {
    base <- sprintf('https://test-files.osf.io/v1/resources/%s/providers/%s/%s',
      id, provider, request)
  } else if (Sys.getenv('OSF_USE_TEST_SERVER') == 'staging') {
    base <- sprintf('https://staging-files.osf.io/v1/resources/%s/providers/%s/%s',
      id, provider, request)
  } else {
    base <- sprintf('https://files.osf.io/v1/resources/%s/providers/%s/%s',
      id, provider, request)
  }

  return(base)
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
#'
#' @return Nothing if succeeded
#' @export

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

#' Function to parse API call
#'
#' @param x Object containing the result of an API call.
#'
#' @return Parsed JSON object in the form of an R object.

process_json <- function(x) {
  rjson::fromJSON(httr::content(x, 'text', encoding = "UTF-8"))
}

#' Identify type of endpoint for id
#'
#' @param id OSF id to check
#' @param private Boolean, in case id is private set to TRUE
#'
#' @return Endpoint of id as character (nodes | files)

process_type <- function(id, private = TRUE) {

  url_osf_nodes <- construct_link(sprintf('nodes/%s', id))
  url_osf_files <- construct_link(sprintf('files/%s', id))

  config <- get_config(private)

  call_nodes <- httr::GET(url_osf_nodes, config)
  call_files <- httr::GET(url_osf_files, config)

  if (!call_nodes$status_code == 200 && !call_files$status_code) {
    stop('Failed. Are you sure you have access to the id or that it is valid?')
  } else if (call_nodes$status_code == 200) {
    res <- process_json(call_nodes)
  } else {
    res <- process_json(call_files)
  }

  if (is.null(res$data$type))
    return('')

  return(res$data$type)
}

#' Create authorization config
#'
#' @param login_required Boolean
#'
#' @return configuration for use in httr request

get_config <- function(login_required) {
  config <- list()

  if (login_required) {
    config <- httr::add_headers(Authorization = sprintf('Bearer %s', login()))
  }

  return(config)
}

#' Check OSF id
#'
#' @param id OSF id (osf.io/xxxx; just xxxx)
#'
#' @return Boolean

is_valid_osf_id <- function(id) {
  grepl('[A-Za-z0-9]{5}', id)
}

#' Materialize Waterbutler URL
#'
#' @param url Waterbutler URL, starts with files.osf.io
#' @param private Boolean, whether or not the file is prvate
#'
#' @return OSF id

process_waterbutler <- function(url, private = TRUE) {

  config <- get_config(private)

  url <- sprintf('%s?meta=', url)
  call <- httr::GET(url, config)

  tmp <- process_json(call)
  res <- gsub(x = tmp$data$attributes$resource, pattern = '/', '')

  return(res)
}

#' Processing a file id to waterbutler
#'
#' The OSF uses a separate API to handle file requests, for some reason. This
#' function helps deal with that.
#'
#' @param id OSF id (osf.io/xxxx; just xxxx)
#' @param private Boolean, if file is private
#'
#' @return Waterbutler URL

process_file_id <- function(id, private = FALSE) {

  config <- get_config(private)

  url_osf <- construct_link(paste0('files/', id))
  call <- httr::GET(url = url_osf, config)
  res <- process_json(call)

  if (call$status_code != 200) {
    stop('Failed to retrieve information. Sure it is public?')
  }

  return(res$data$links$download)
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
