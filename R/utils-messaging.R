#' Bullet message
#'
#' Send an error, warning, or message, followed by a markdown-style bullet list.
#' This is frequently used to let users know something happened (or didn't),
#' and provide a list of the relevant files.
#' @param message The message to display
#' @param bullets A character vector where each item is printed following a
#'   bullet point.
#'
#' @examples
#' bullet_msg("Can't download:", bullets = c("file1.txt", "file2.txt"))
#' @noRd
bullet_msg <- function(message, bullets) {
  msg <- paste(c(
    message,
    sprintf("  * %s", bullets)),
    collapse = "\n"
  )
  return(msg)
}
