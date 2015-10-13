#' Retrieve nodes associated with id
#'
#' @param id The id to search for. Use `NULL` to retrieve all,
#' `me` for logged in account. Maximum of 1 id.
#' @param user Username to log in with
#' @param password Password to log in with
#' @param contributors Boolean to extract the contributors of the node
#' @param files Boolean to extract links
#'
#' @return List object of results
#'
#' @examples
#' get.nodes()
#' get.nodes(id = 'me', user = 'h.schwarzenegger@gmail.com', 'testingtesting')
#' get.nodes(id = 'nu97z')
get.nodes <- function(id = NULL,
                      user = NULL,
                      password = NULL,
                      contributors = FALSE,
                      files = FALSE,
                      children = FALSE){
  if (is.null(id)){
    call <- httr::GET(construct.link('nodes'))
  } else if (id == 'me'){
    if(is.null(user)) stop("Requires user")
    if(is.null(password)) stop("Requires password")

    call <- httr::GET(construct.link('nodes'),
                      httr::authenticate(user, password))
  } else {
    call <- httr::GET(construct.link(paste('nodes', id, sep = '/')))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))

  if (names(res) == "errors" & !is.null(id)) stop("Node not found.")
  if (sum(c(contributors, files, children)) > 1){stop("Specify contributors OR files OR children")}

  if (contributors == TRUE){
    call <- httr::GET(res$data$relationships$contributors$links$related$href)
    res <- rjson::fromJSON(httr::content(call, 'text'))
  }
  if (files == TRUE){
    call <- httr::GET(res$data$relationships$files$links$related$href)
    res <- rjson::fromJSON(httr::content(call, 'text'))
  }
  if (children == TRUE){
    call <- httr::GET(res$data$relationships$children$links$related$href)
    res <- rjson::fromJSON(httr::content(call, 'text'))
  }

  return(res)
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
