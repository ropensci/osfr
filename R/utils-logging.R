log_response <- function(res) {
  if (!is.null(getOption("osfr.log"))) {
    logger::log_info(fmt = "%s %s", toupper(res$method), res$request$url$url)
  }
  res
}
