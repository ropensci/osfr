#' @importFrom cli cat_line
print.osf_file <- function(x, ...) {
  cli::cat_line(x)

  attrs <- attr(x, "attributes")
  modified <- as.POSIXct(attrs$modified_utc, "%FT%T+00:00", tz = "UTC")

  info <- c(
    paste("  name:", attrs$name),
    paste("  parent:", attrs$resource),
    paste("  modified:", strftime(modified, "%x %X"))
  )

  cli::cat_line(info, col = "grey")
}


