log_response <- function(res) {
  logfile <- getOption("osfr.log")
  if (is.null(logfile)) return(res)
  logger::log_appender(logger::appender_file(logfile))
  logger::log_formatter(logger::formatter_sprintf)
  logger::log_info(fmt = "%s %s", toupper(res$method), res$request$url$url)
  res
}
