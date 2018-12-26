.osf_api_version <- 2.8
.wb_api_version  <- 1

.onLoad <- function(libname, pkgname) {
  env.pat <- Sys.getenv("OSF_PAT")
  if (nzchar(env.pat)) options(osfr.pat = env.pat)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!is.null(getOption("osfr.pat"))) {
    packageStartupMessage("Automatically registered OSF personal access token")
  }
}
