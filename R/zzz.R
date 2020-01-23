.onLoad <- function(libname, pkgname) { # nolint

  # record personal access token
  env_pat <- Sys.getenv("OSF_PAT")
  if (nzchar(env_pat)) options(osfr.pat = env_pat)

  # setup logger
  env_log <- Sys.getenv("OSF_LOG")
  if (nzchar(env_log)) {
    if (requireNamespace("logger", quietly = TRUE)) {
      options(osfr.log = env_log)
      logger::log_appender(logger::appender_file(env_log), namespace = "osfr")
      logger::log_formatter(logger::formatter_sprintf, namespace = "osfr")
    }
  }

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

.onAttach <- function(libname, pkgname) { # nolint
  if (!is.null(getOption("osfr.pat"))) {
    packageStartupMessage("Automatically registered OSF personal access token")
  }

  if (is.null(getOption("osfr.log")) && nzchar(Sys.getenv("OSF_LOG"))) {
    packageStartupMessage(
      "<Logging NOT enabled: please install the logger package>"
    )
  }

  if (!is.null(getOption("osfr.log"))) {
    packageStartupMessage(
      sprintf("<Logging enabled: %s>", getOption("osfr.log"))
    )
  }

  server <- Sys.getenv("OSF_SERVER")
  if (nzchar(server)) {
    packageStartupMessage(
      sprintf("<Testing server enabled: %s.osf.io>", tolower(server))
    )
  }
}
