#' Encode and Decode HTML Entity References
#'
#' A select number of characters are encoded as HTML entity references when
#' they're included in node names (they are left as is in file/directory names.)
#'
#' @param x scalar character vector containing HTML symbols to encoded/decoded
#'
#' @noRd

.html_entities  <- list(
  symbol = c("&",     "<",    ">"),
  name   = c("&amp;", "&lt;", "&gt;")
)

html_decode <- function(x) {
  for (i in seq_along(.html_entities$symbol)) {
    x <- gsub(.html_entities$name[i], .html_entities$symbol[i], x, fixed = TRUE)
  }
  x
}

html_encode <- function(x) {
  for (i in seq_along(.html_entities$symbol)) {
    x <- gsub(.html_entities$symbol[i], .html_entities$name[i], x, fixed = TRUE)
  }
  x
}
