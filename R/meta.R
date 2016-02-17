#' Welcome function
#'
#' @param user User account
#' @param password Password
#'
#' @return Information about OSF version and user in list format

welcome <- function(user = NULL, password = NULL){
  if (is.null(user) | is.null(password)){
    call <- httr::GET(url = construct.link())
  } else {
    call <- httr::GET(url = construct.link(), httr::authenticate(user, password))}

  res <- rjson::fromJSON(httr::content(call, 'text'))

  if (is.null(res$meta$current_user)) warning("Currently not logged in\n")
  if (!is.null(res$meta$current_user)) cat(sprintf("Welcome user_id %s",
                                                   res$meta$current_user$data$id))

  return(res)
}

#' Construct an API link with proper base

#' @param request The request link to be combined with the base API link.
#' @return The full request link with proper base
#' @examples
#' construct.link("nodes/{node_id}/files/")

construct.link <- function(request = NULL, login = FALSE){
  base <- "https://test-api.osf.io/v2/"

  result <- paste0(base, request)

  if (login == TRUE){
    result <- paste0("https://test-accounts.osf.io/login/oauth2", request)
  }

  return(result)
}

login <- function(){
  if (Sys.getenv("OSF_PAT") == ""){
    input <- readline(prompt = "Visit https://osf.io/settings/tokens/ and create a Personal access token: ")

    Sys.setenv(OSF_PAT = input)
  }

  return(Sys.getenv("OSF_PAT"))
}

logout <- function(){
  if (Sys.getenv("OSF_PAT") == ""){
    cat("Not logged in.")
  } else{
    Sys.unsetenv("OSF_PAT")

    cat("Successfully logged out. Use login() to log back in.")
  }
}
