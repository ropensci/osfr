.osf_api_version <- 2.8
.wb_api_version  <- 1

.onLoad <- function(libname, pkgname) {
  env.pat <- Sys.getenv("OSF_PAT")
  if (nzchar(env.pat)) {
    packageStartupMessage("Automatically registered OSF personal access token")
    options(osfr.pat = env.pat)
  }
  invisible()
}
