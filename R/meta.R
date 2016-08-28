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
    call <- httr::GET(url = construct.link(...))
  } else
  {
    call <- httr::GET(url = construct.link(...),
                      httr::add_headers(Authorization = sprintf("Bearer %s", login())))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))

  if (is.null(res$meta$current_user))
  {
    warning("Currently not logged in\n")
  }
  if (!is.null(res$meta$current_user))
  {
    cat(sprintf("Welcome user_id %s", res$meta$current_user$data$id))
  }
}

#' Construct an API link with proper base

#' @param request The request link to be combined with the base API link.
#'
#' @return The full request link with proper base
#' @examples
#' construct.link("nodes/{node_id}/files/")

construct.link <- function(request = NULL, test = FALSE){
  if (test == FALSE)
    base <- "https://api.osf.io/v2/"
  if (test == TRUE)
    base <- "https://test-api.osf.io/v2/"
  result <- paste0(base, request)

  return(result)
}

#' Login function; interactive without arguments
#'
#' @param pat Personal Access Token (PAT) for fast login.
#'
#' @return Personal access token from global environment.
#' @export

login <- function(pat = NULL){
  if (!is.null(pat)) Sys.setenv(OSF_PAT = pat) else{
    if (Sys.getenv("OSF_PAT") == ""){
      input <- readline(prompt = "Visit https://osf.io/settings/tokens/ and create a Personal access token: ")

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

logout <- function(){
  if (Sys.getenv("OSF_PAT") == ""){
    cat("Not logged in.")

    return(FALSE)
  } else{
    Sys.unsetenv("OSF_PAT")

    cat("Successfully logged out. Use login() to log back in.")

    return(TRUE)
  }
}
