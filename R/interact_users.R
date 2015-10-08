get.users <- function(id = NULL, user = NULL, password = NULL, nodes = FALSE){
  if (is.null(id)){
    raw <- httr::GET(construct.link("users"))

    result <- rjson::fromJSON(content(raw, 'text'))
  } else if (id == "me"){
    if(is.null(user)){
      warning("Please input username")}
    if(is.null(password)){
      warning("Please input password")}
    if(nodes == TRUE){
      raw <- httr::GET(construct.link("users/me/nodes"),
                       httr::authenticate(user, password))
    } else {
      raw <- httr::GET(construct.link("users/me"),
                       httr::authenticate(user, password))
    }

    result <- rjson::fromJSON(content(raw, 'text'))
  } else {
    if(nodes == TRUE){
      raw <- httr::GET(construct.link(paste0("users/?filter[id]=", id, "/nodes")))
    } else{
      raw <- httr::GET(construct.link(paste0("users/?filter[id]=", id)))
    }

    result <- rjson::fromJSON(content(raw, 'text'))
  }

  if (names(results) == 'errors'){
    stop("Incorrect password")
  }

  return(result)
}

put.users <- function(id = NULL, user = NULL, password = NULL){

  # Replace the 'me' string with the actual id
  if(id == 'me'){
    id <- get.users(id = id, user = user, password = password)$data$id}
}

patch.users <- function(id = 'me',
                        user = NULL,
                        password = NULL,
                        full_name = NULL,
                        given_name = NULL,
                        middle_names = NULL,
                        family_name = NULL,
                        suffix = NULL){
  # To prevent errors due to not being logged in
  if (is.null(user)){
    stop("Please input username")}
  if (is.null(password)){
    stop("Please input password")}
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

  temp <- httr::PATCH(url = link, body = edits, httr::authenticate(user, password))

  if (!temp$status_code == 200){
    cat(sprintf('Patch of user %s failed, errorcode %s\n',
                id, temp$status_code))
    res <- FALSE
  } else {
    cat(sprintf('Patch of user %s succeeded\n', id))
    res <- TRUE}

  return(res)
}
