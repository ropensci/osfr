#' Personalised welcome
#'
#' Function that helps you identify who's logged in, just in case you forget.
#'
#' @return Welcome message of logged in user, if any
#' @export

welcome <- function() {
  if (Sys.getenv('OSF_PAT') == '') {
    login()
    call <- httr::GET(url = construct_link())
  } else {
    config <- get_config(TRUE)
    call <- httr::GET(url = construct_link(), config)
  }

  res <- process_json(call)

  if (is.null(res$meta$current_user)) {
    warning('Currently not logged in\n')
  } else if (!is.null(res$meta$current_user)) {
    message(sprintf('Welcome %s\n', res$meta$current_user$data$attributes$full_name))
  } else {
    stop('Something unexpected occurred. Blame the developer! BOO.\n')
  }
}

#' Login function; interactive without arguments
#'
#' Function to easily login, either with a prompt when run without arguments, or
#' without prompt when run with arguments or config file (~/.osf_config). If you
#' run this function without arguments, the prompt will result in creation of a
#' ~/.osf_config file (in your Documents folder for Windows).
#'
#' For the security of your OSF account, do not forget to delete this file. This
#' can be done with the logout() function as well.
#'
#' @param pat Personal Access Token (PAT) for fast login. If no pat is given, function queries for it.
#'
#' @return Personal access token from global environment.
#' @export

login <- function(pat = NULL) {
  if (!is.null(Sys.getenv('OSF_PAT'))) {
    # do nothing, sufficient for auth purposes
  } else if (!is.null(pat)) {
    Sys.setenv(OSF_PAT = pat)
  } else if (Sys.getenv('OSF_PAT') == '' && file.exists('~/.osf_config')) {
      Sys.setenv(OSF_PAT = readLines('~/.osf_config')[1])
  } else if (Sys.getenv('OSF_PAT') != '') {
    NULL
  } else {
      input <- readline(prompt = 'Visit https://osf.io/settings/tokens/
                  and create a Personal access token: ')
      Sys.setenv(OSF_PAT = input)

      # Write to file in a normalized manner across UNIX and Windows
      connect <- file(normalizePath('~/.osf_config'))
      writeLines(input, connect)
      close(connect)
  }

  invisible(Sys.getenv('OSF_PAT'))
}

#' Logout function
#'
#' Log out of the OSF and clean up after yourself for your own security. This
#' ensures that you do not leave a personal access token lying around on the
#' computer for others to abuse without your knowledge (before revoking it,
#' but then the damage is already done).
#'
#' @return Boolean succes of logout
#' @export

logout <- function() {
  if (Sys.getenv('OSF_PAT') == '') {
    message('Not logged in.')
    return(FALSE)
  } else {
    Sys.setenv(OSF_PAT = '')
    try(file.remove(normalizePath('~/.osf_config')))
    return(TRUE)
  }
}
