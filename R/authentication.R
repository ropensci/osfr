#' Login to the OSF
#'
#' Login to the Open Science Framework API with a Personal Access Token (PAT).
#' By default and for security reasons, the login lasts as long as the current
#' session. If you want to store the login across sessions, use argument 
#' \code{store} (this saves the PAT in your .Renviron; see also 
#' \link{\code{logout}}).
#'
#' @param pat Personal Access Token (PAT) for fast login. If no pat is given, function queries for it.
#' @param store If TRUE, store PAT in user's .Renviron.
#'
#' @export

login <- function (pat, store = FALSE) {
  # set pat as environment variable
  Sys.setenv(OSF_PAT = pat)
  # store to rprofile for permanency
  if (store) {
    write(sprintf('OSF_PAT = %s', pat),
      normalizePath('~/.Renviron', mustWork = TRUE),
      append = TRUE)
  }
}

#' Logout of the OSF
#'
#' Remove the Personal Access Token that provides access to the Open Science 
#' Framework API. Removes the \code{OSF_PAT} from session environment, not 
#' from the .Renviron file if you stored it with login (that is currently a
#' manual step).
#' 
#' @export

logout <- function () {
  Sys.setenv(OSF_PAT = '')
}

#' Authentication function
#' 
#' Simple helper function to return the PAT in HTTP requests.

auth <- function () {
  if (Sys.getenv('OSF_PAT') == '') {
    stop('Please log in first.')
  }
  invisible(Sys.getenv('OSF_PAT'))
}