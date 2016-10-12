#' Welcome function
#'
#' @param ... other arguments, only for testing.
#'
#' @return Welcome message of logged in user, if any
#' @export

welcome <- function(...)
{
  if (Sys.getenv("OSF_PAT") == "")
  {
    login()
    call <- httr::GET(url = construct_link(...))
  } else
  {
    call <- httr::GET(url = construct_link(...),
                      httr::add_headers(Authorization = sprintf("Bearer %s", login())))
  }

  res <- process_json(call)

  if (is.null(res$meta$current_user))
  {
    warning("Currently not logged in\n")
  }
  if (!is.null(res$meta$current_user))
  {
    cat(sprintf("Welcome %s", res$meta$current_user$data$attributes$full_name))
  }

  return(res)
}

#' Construct an API link with proper base

#' @param request The request link to be combined with the base API link.
#'
#' @return The full request link with proper base
#' @examples
#' construct_link("nodes/{node_id}/files/")

construct_link <- function(request = NULL,
                           test = FALSE){
  if (test == FALSE)
  {
    base <- "https://api.osf.io/v2/"
  }
  if (test == TRUE)
  {
    base <- "https://test-api.osf.io/v2/"
  }

  result <- paste0(base, request)

  return(result)
}

#' Login function; interactive without arguments
#'
#' @param pat Personal Access Token (PAT) for fast login. If no pat is given, function queries for it.
#'
#' @return Personal access token from global environment.
#' @export

login <- function(pat = NULL){
  if (!is.null(pat))
  {
    Sys.setenv(OSF_PAT = pat)
  } else
  {
    if (Sys.getenv("OSF_PAT") == ""){
      input <- readline(prompt = "Visit https://osf.io/settings/tokens/
                        and create a Personal access token: ")

      Sys.setenv(OSF_PAT = input)

      if (file.exists(paste0(normalizePath('~/'), '.Renviron')))
      {
        write(sprintf('OSF_PAT=%s', input),
              paste0(normalizePath('~/'), '/.Renviron'),
              append = TRUE)
      } else
      {
        write(sprintf('OSF_PAT=%s', input),
              paste0(normalizePath('~/'), '/.Renviron'),
              append = FALSE)
      }
    }
  }

  return(Sys.getenv("OSF_PAT"))
}


#' Logout function
#'
#' @return Logical of successfulness of logout
#' @export

logout <- function(...)
{
  if (Sys.getenv("OSF_PAT") == "")
  {
    cat("Not logged in.")

    return(FALSE)
  } else
  {
    Sys.unsetenv("OSF_PAT")

    return(TRUE)
  }
}

process_json <- function(x)
{
  res <- rjson::fromJSON(httr::content(x, 'text', encoding = "UTF-8"))

  return(res)
}

check_type <- function(id = NULL,
                       private = FALSE,
                       ...)
{
  if(is.null(id)) stop('Enter id to check.')

  url.osf.nodes <- construct_link(sprintf('nodes/%s', id), ...)
  url.osf.files <- construct_link(sprintf('files/%s', id), ...)

  if (private == TRUE)
  {
    if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')

    call_nodes <- httr::GET(url.osf.nodes,
                      httr::add_headers(Authorization = sprintf(
                        'Bearer %s',
                        login())))
    call_files <- httr::GET(url.osf.files,
                            httr::add_headers(Authorization = sprintf(
                              'Bearer %s',
                              login())))
  } else
  {
    call_nodes <- httr::GET(url.osf.nodes)
    call_files <- httr::GET(url.osf.files)
  }

  if (!call_nodes$status_code == 200 & !call_files$status_code){
    stop('Failed. Sure you have access to the id or that it is valid?')
  } else if (call_nodes$status_code == 200)
  {
    res <- process_json(call_nodes)
  } else
  {
    res <- process_json(call_files)
  }

  return(res$data$type)
}
