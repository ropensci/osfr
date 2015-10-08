#' Retrieve nodes viewable to the httr::authenticated account on the OSF.

#' @return Object dataframe including, for each node:
#' \enumerate{
#' \item id
#' \item title
#' \item description
#' \item category
#' \item date_created
#' \item date_modified
#' \item tags
#' \item registration
#' \item collection
#' \item dashboard
#' \item links
#' \item public
#' \item children
#' \item contributors
#' \item files
#' \item node_links
#' \item parent
#' \item registrations
#' }

get.nodes <- function(id = NULL){
  if (is.null(id)){
    raw <- httr::GET(construct.link("nodes"))

    result <- rjson::fromJSON(content(raw, 'text'))
  } else {
    raw <- httr::GET(construct.link(paste("nodes", id, sep = '/')))

    result <- rjson::fromJSON(content(raw, 'text'))
  }

  return(result)
}

post.nodes <- function(){}

put.nodes <- function(){}

patch.nodes <- function(){}

#' Delete a node with its id
#'
#' @param id The node_id to be deleted.
#' @param user The username to log in with (temporary until OAUTH2.0)
#' @param password The password to log in with (temporary until OAUTH2.0)
#' @return Boolean TRUE if deletion succeeded

delete.nodes <- function(id = NULL, user = NULL, password = NULL){
  if(is.null(id)){
    break('Please input node to delete')}
  if(is.null(user)){
    warning("Please input username")}
  if(is.null(password)){
    warning("Please input password")}
  link <- construct.link(paste("nodes", id, sep = "/"))

  temp <- httr::DELETE(link, httr::authenticate(user, password))

  if(!temp$status_code == 204){
    cat(sprintf('Deletion of node %s failed, errorcode %s\n',
                id, temp$status_code))
    res <- FALSE
  } else {
    cat(sprintf('Deletion of node %s succeeded\n', id))
    res <- TRUE}

  return(res)
}
