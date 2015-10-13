get.nodes <- function(id = NULL, user = NULL, password = NULL){
  if(is.null(user)){
    warning("Please input username if you also want to view private nodes")}
  if(is.null(password)){
    warning("Please input password if you also want to view private nodes")}

  if (is.null(id)){
    raw <- httr::GET(construct.link("nodes"))

    result <- rjson::fromJSON(httr::content(raw, 'text'))
  } else {
    raw <- httr::GET(construct.link(paste("nodes", id, sep = '/')))

    result <- rjson::fromJSON(httr::content(raw, 'text'))
  }

return(result)
}

post.nodes <- function(user = NULL,
                       password = NULL,
                       type = 'nodes',
                       title = NULL,
                       description = NULL,
                       category = 'project',
                       tags = NULL,
                       public = 'true'){
  if(is.null(user)){
    stop("Requires username")}
  if(is.null(password)){
    stop("Requires password")}
  if(is.null(title)){
    stop("Requires title")}
  if(!category %in% c('project',
                        'hypothesis',
                        'methods and measures',
                        'procedure',
                        'instrumentation',
                        'data',
                        'analysis',
                        'communication',
                        'other')){
    stop("Please input proper category, see documentation")}

  link <- construct.link('nodes')

  edits <- list(type = type,
                attributes = list(
                title = title,
                description = description,
                category = category,
                tags = tags,
                public = public))

  temp <- httr::POST(url = link,
                     body = edits,
                     httr::authenticate(user, password))

  if (!temp$status_code == 201){
    stop(sprintf("Creation of new %s failed", ifelse(category == 'project',
                                                    'project',
                                                    'component')))
  }

  re

}

put.nodes <- function(){}

patch.nodes <- function(){}

#' Delete a node with its id
#'
#' @param id The node_id to be deleted.
#' @param user The username to log in with (temporary until OAUTH2.0)
#' @param password The password to log in with (temporary until OAUTH2.0)
#' @return Boolean TRUE if deletion succeeded

delete.nodes <- function(id = NULL, user = NULL, password = NULL){
  if (is.null(id)){
    break('Please input node to delete')}
  if (is.null(user)){
    warning("Please input username")}
  if (is.null(password)){
    warning("Please input password")}
  link <- construct.link(paste("nodes", id, sep = "/"))

  temp <- httr::DELETE(link, httr::authenticate(user, password))

  if (!temp$status_code == 204){
    cat(sprintf('Deletion of node %s failed, errorcode %s\n',
                id, temp$status_code))
    res <- FALSE
  } else {
    cat(sprintf('Deletion of node %s succeeded\n', id))
    res <- TRUE}

  return(res)
}
