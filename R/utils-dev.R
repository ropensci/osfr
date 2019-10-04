#' Convenience functions for developers to switch between accounts
#'
#' Use these functions to:
#' * switch between OSF's test and production servers
#' * switch between your development and standard PAT
#'
#' Assumes your home directory contains a `.Renviron` file that defines
#' `OSF_PAT` with your standard PAT, and your current working directory contains
#' another `.Renviron` file with the PAT you use for `test.osf.io`.
#' @noRd
NULL

osf_dev_on <- function() {
  renviron <- normalizePath(".Renviron")
  stopifnot(file.exists(renviron))
  stopifnot(readRenviron(renviron))
  Sys.setenv(OSF_SERVER = "test")
  message("osfr development mode enabled.")
  osf_auth()
}

osf_dev_off <- function() {
  renviron <- normalizePath("~/.Renviron")
  stopifnot(file.exists(renviron))
  stopifnot(readRenviron(renviron))
  Sys.unsetenv("OSF_SERVER")
  message("osfr development mode disabled.")
  osf_auth()
}
