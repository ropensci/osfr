#' Function to parse API call
#'
#' @param x Object containing the result of an API call.
#'
#' @return Parsed JSON object in the form of an R object.

process_json <- function(x)
{
  res <- rjson::fromJSON(httr::content(x, 'text', encoding = "UTF-8"))

  return(res)
}

