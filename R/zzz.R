.osf_api_version <- 2.8
.wb_api_version  <- 1

.onLoad <- function(libname, pkgname) {

  # record personal access token
  env.pat <- Sys.getenv("OSF_PAT")
  if (nzchar(env.pat)) options(osfr.pat = env.pat)

  # register dplyr methods
  if (requireNamespace("dplyr", quietly = TRUE)) {
    register_s3_method("dplyr", "arrange", "osf_tbl")
    register_s3_method("dplyr", "filter",  "osf_tbl")
    register_s3_method("dplyr", "mutate",  "osf_tbl")
    register_s3_method("dplyr", "rename",  "osf_tbl")
    register_s3_method("dplyr", "select",  "osf_tbl")
    register_s3_method("dplyr", "slice",   "osf_tbl")
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!is.null(getOption("osfr.pat"))) {
    packageStartupMessage("Automatically registered OSF personal access token")
  }

  server <- Sys.getenv("OSF_SERVER")
  if (!nzchar(server)) {
    packageStartupMessage(
      sprintf("<Testing server enabled: %s.osf.io>", tolower(server))
    )
  }
}
