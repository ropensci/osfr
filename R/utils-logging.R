log_response <- function(res) {
  if (!is.null(getOption("osfr.log"))) {
    times <- res$times[res$times > 0]

    msg <- sprintf(fmt = "STATUS %s - %s - [%s]",
      res$status_code,
      res$url,
      paste0(sprintf("%s:%f", names(times), times), collapse = ", ")
    )

    if (res$status_code >= 500) {
      msg <- paste(msg, rawToChar(res$content), sep = " - ")
      logger::log_error("%s", msg)
    } else {
      logger::log_info("%s", msg)
    }
  }
}


log_request <- function(req) {
  if (!is.null(getOption("osfr.log"))) {
    logger::log_info(fmt = "%s %s", toupper(req$method), req$url$url)
  }
}
