#' OSF User Authentication
#'
#' Authorize \pkg{osfr} to interact with your OSF data and OSF account by
#' passing a personal access token (PAT) to \code{osf_auth()}. If no \var{token}
#' is provided, \code{osf_auth()} will attempt to obtain a PAT from the
#' \env{OSF_PAT} environment variable. However, since \pkg{osfr} checks for the
#' presence of \env{OSF_PAT} on start-up, this is only necessary if the variable
#' was created or redefined in the middle of a session. See below for additional
#' details and instructions for generating and utilizing your PAT.
#'
#' Out of the box \pkg{osfr} can only access publicly available projects,
#' components, and files on OSF. In order for \pkg{osfr} to view and manage your
#' private resources you must provide a Personal Access Token (PAT). The
#' following instructions will walk you through the process of generating a PAT
#' and using it to authenticate \pkg{osfr}.
#'
#' @param token OSF personal access token
#' @examples
#' osf_auth("a22JKLgdAEx9wms...")
#'
#' @return Invisibly returns your OSF PAT
#'
#' @section Creating an OSF Personal Access Token:
#'
#' \enumerate{
#'   \item Navigate to \url{https://osf.io/settings/tokens/}
#'   \item Click the \emph{New token} button and provide a descriptive name
#'   \item Select the scopes (i.e., permissions) you'd like to grant \pkg{osfr}
#'   \item Click the \emph{Create} button to generate your PAT
#'   \item If successful, your 70 character token will be displayed along with several important warnings you should definitely read over carefully
#'   \item You read those warnings, right?
#'   \item Copy your token and keep it in a safe place
#' }
#'
#' @section Using your PAT with \pkg{osfr}:
#'
#' There are two possible approaches for authenticating \pkg{osfr} with your PAT.
#'
#' The simpler approach is to call the \code{osf_auth()} function at the start
#' of every new R session and manually paste in your token. Note that your PAT
#' should be treated like a password and, as such, should not be hardcoded into
#' your script.
#'
#' A better approach is to store your PAT as an environment variable called
#' \var{OSF_PAT}. Doing so will allow \pkg{osfr} to detect and utilize the token
#' automatically without any need to manually call \code{osf_auth()}. One way to
#' accomplish this is by creating an \file{.Renviron} file in your home or
#' working directory that defines the \var{OSF_PAT} variable. For example:
#'
#' \preformatted{
#' OSF_PAT=a22JKLgdAEx9wms...
#' }
#'
#' To verify this was done correctly restart R and run
#' \code{Sys.getenv("OSF_PAT")}, which should return your PAT.
#'
#' @references
#' Colin Gillespie and Robin Lovelace (2017). \emph{Efficient R programming}.
#' O'Reilly Press. \url{https://csgillespie.github.io/efficientR}.
#'
#' @export
osf_auth <- function(token = NULL, verbose = TRUE) {

  if (is.null(token)) {
    env_pat <- Sys.getenv("OSF_PAT")
    if (nzchar(env_pat)) token <- env_pat
    source <- "OSF_PAT environment variable"
  } else {
    stopifnot(is.character(token))
    source <- "provided token"
  }

  if (verbose) {
    if (is.null(token)) {
      warning("No PAT found. See ?osf_auth for help")
    } else {
      message("Registered PAT from the ", source)
    }
  }

  options(osfr.pat = token)
  invisible(token)
}

