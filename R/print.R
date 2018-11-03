#' @importFrom cli cat_line
print.osf_file <- function(x, ...) {
  cli::cat_line(x)

  attrs <- attr(x, "attributes")
  modified <- as.POSIXct(attrs$modified_utc, "%FT%T+00:00", tz = "UTC")

  info <- c(
    "<osf_file>",
    paste("  name:", attrs$name),
    paste("  parent:", attrs$resource),
    paste("  modified:", strftime(modified, "%x %X"))
  )

  cli::cat_line(info, col = "grey")
}


print.osf_project <- function(x, ...) {
  cli::cat_line(x)

  attrs <- attr(x, "attributes")
  modified <- as.POSIXct(attrs$date_modified, "%FT%T", tz = "UTC")

  info <- c(
    "<osf_project>",
    paste("  title:", attrs$title),
    paste("  public:", ifelse(attrs$public, cli::symbol$tick, cli::symbol$cross)),
    paste("  modified:", strftime(modified, "%x %X"))
  )

  cli::cat_line(info, col = "grey")
}

