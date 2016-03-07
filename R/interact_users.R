#' Return information on an OSF user
#'
#' @param id Argument to specify the OSF id ('me' refers to logged in account)
#' @param nodes Boolean to return the nodes available for that user
#'
#' @return List object with account information
#' @export

get.users <- function(id = NULL, nodes = FALSE){
  if (Sys.getenv("OSF_PAT") == "" & is.null(id)){
    raw <- httr::GET(construct.link("users"))

    result <- rjson::fromJSON(httr::content(raw, 'text'))
  } else if (id == "me"){

    if(Sys.getenv("OSF_PAT") == ""){
      warning("Please login first using the login() function")}

    if(nodes == TRUE){
      raw <- httr::GET(construct.link("users/me/nodes"),
                       httr::add_headers(Authorization = sprintf("Bearer %s", login())))
    } else {
      raw <- httr::GET(construct.link("users/me"),
                       httr::add_headers(Authorization = sprintf("Bearer %s", login())))
    }

    result <- rjson::fromJSON(httr::content(raw, 'text'))
  } else {
    if(nodes == TRUE){
      raw <- httr::GET(construct.link(paste0("users/?filter[id]=", id, "/nodes")))
    } else{
      raw <- httr::GET(construct.link(paste0("users/?filter[id]=", id)))
    }

    result <- rjson::fromJSON(httr::content(raw, 'text'))
  }

  while (!is.null(result$links$`next`)){
    whilst <- rjson::fromJSON(
      httr::content(
        httr::GET(
          result$links$`next`),
        'text'))
    result$data <- c(result$data, whilst$data)
    result$links$`next` <- whilst$links$`next`
    cat(paste0(result$links$`next`, '\n'))
  }

  return(result)
}

#' Put user information
#'
#' @param id the id to put information for
#' @param type type of id, 'users' only option
#' @param full_name
#' @param given_name
#' @param middle_names
#' @param family_name
#' @param suffix
#'
#' @return Boolean indicating success of PUTting user information
#' @export

put.users <- function(id = 'me',
                      type = "users",
                      full_name = NULL,
                      given_name = NULL,
                      middle_names = NULL,
                      family_name = NULL,
                      suffix = NULL){
  if (Sys.getenv("OSF_PAT") == "") stop("Please login using the login() function")
  if (is.null(id)) stop("Please input an id")
  if (!(class(id) == 'character' & length(id) == 1)){
    stop('Please use characters and specify only ONE id')}

  # Replace the 'me' string with the actual id
  if(id == 'me'){
    id <- get.users(id = id)$data$id}

  link <- construct.link(paste0(type, '/', id))

  edits <- list(type = type,
                id = id,
                full_name = full_name,
                given_name = given_name,
                middle_names = middle_names,
                family_name = family_name,
                suffix = suffix)

  temp <- httr::PATCH(url = link, body = edits,
                      httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (!temp$status_code == 200){
    cat(sprintf('Put of user %s failed, errorcode %s\n',
                id, temp$status_code))
    results <- FALSE
  } else {
    cat(sprintf('Put of user %s succeeded\n', id))
    results <- TRUE}

  return(results)
}

#' Patch user information
#'
#' @param id
#' @param full_name
#' @param given_name
#' @param middle_names
#' @param family_name
#' @param suffix
#'
#' @return Boolean of success of patching user information
#' @export

patch.users <- function(id = 'me',
                        full_name = NULL,
                        given_name = NULL,
                        middle_names = NULL,
                        family_name = NULL,
                        suffix = NULL){
  # To prevent errors due to not being logged in
  if (Sys.getenv("OSF_PAT") == '') stop("Please login using the login() function")
  if (is.null(id)){
    stop("Please input an id")}
  if (!(class(id) == 'character' & length(id) == 1)){
    stop('Please use characters and specify only ONE id')}

  link <- construct.link(paste0('users/', id))

  edits <- list(full_name = full_name,
                given_name = given_name,
                middle_names = middle_names,
                family_name = family_name,
                suffix = suffix)

  temp <- httr::PATCH(url = link, body = edits,
                      httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (!temp$status_code == 200){
    cat(sprintf('Patch of user %s failed, errorcode %s\n',
                id, temp$status_code))
    results <- FALSE
  } else {
    cat(sprintf('Patch of user %s succeeded\n', id))
    results <- TRUE}

  return(results)
}
