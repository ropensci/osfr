#' Stop execution with HTTP status code
#' @param code HTTP status code
#' @inheritParams base::stop
http_error <- function(code, ...) {
  args <- list(...)
  msg <- sprintf("\n       HTTP status code %i.", code)
  stop(args, msg, call. = FALSE)
}
