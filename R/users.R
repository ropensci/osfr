#' Return information on an OSF user
#'
#' @param id Argument to specify the OSF id ('me' refers to logged in account)
#' @param nodes Boolean to return the nodes available for that user
#'
#' @return List object with account information
#' @export
get_users <- function(id = NULL, nodes = FALSE) {

  if (Sys.getenv("OSF_PAT") == "" && is.null(id)) {
    raw <- httr::GET(construct_link("users"))
    result <- rjson::fromJSON(httr::content(raw, "text"))
  } else if (id == "me"){
    if (Sys.getenv("OSF_PAT") == "")
      warning("Please login first using the login() function")

    if (nodes == TRUE) {
      raw <- httr::GET(
        construct_link("users/me/nodes"),
        httr::add_headers(Authorization = sprintf("Bearer %s", login())))
    } else {
      raw <- httr::GET(
        construct_link("users/me"),
        httr::add_headers(Authorization = sprintf("Bearer %s", login())))
    }

    result <- rjson::fromJSON(httr::content(raw, "text"))
  } else {
    if (nodes == TRUE){
      raw <- httr::GET(construct_link(paste0("users/?filter[id]=", id, "/nodes")))
    } else {
      raw <- httr::GET(construct_link(paste0("users/?filter[id]=", id)))
    }

    result <- rjson::fromJSON(httr::content(raw, "text"))
  }

  while (!is.null(result$links$`next`)){
    whilst <- rjson::fromJSON(
      httr::content(
        httr::GET(result$links$`next`), "text"))
    result$data <- c(result$data, whilst$data)
    result$links$`next` <- whilst$links$`next`
    message(paste0(result$links$`next`))
  }

  return(result)
}
