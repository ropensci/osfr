#' Construct a Waterbutler API link with proper base
#'
#' @param id OSF id
#' @param provider Storage provider (osfstorage, github, etc)
#' @param request Request for waterbutler
#'
#' @return Waterbutler link

construct_link_files <- function(id = NULL, provider = 'osfstorage', request = NULL) {
  if (Sys.getenv('OSF_USE_SERVER') == 'test') {
    base <- sprintf('https://files.us.test.osf.io/v1/resources/%s/providers/%s/%s',
      id, provider, request)
  } else if (Sys.getenv('OSF_USE_SERVER') == 'staging') {
    base <- sprintf('https://staging-files.osf.io/v1/resources/%s/providers/%s/%s',
      id, provider, request)
  } else {
    base <- sprintf('https://files.osf.io/v1/resources/%s/providers/%s/%s',
      id, provider, request)
  }

  return(base)
}

#' Materialize Waterbutler URL
#'
#' @param url Waterbutler URL, starts with files.osf.io
#' @param private Boolean, whether or not the file is private
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
#' This function retrieves a WaterButler link for a given file id.
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

  return(res$data$links$move)
}
