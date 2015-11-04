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

  if (names(res)[1] == "errors" & !is.null(id)) stop("Node not found.")
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

  while (!is.null(res$links$`next`)){
    whilst <- rjson::fromJSON(
      httr::content(
        httr::GET(
          res$links$`next`),
        'text'))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    cat(paste0(res$links$`next`, '\n'))
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

  link <- construct.link('nodes/')

  if (is.null(description) & is.null(tags)){
    edits <- list(data = list(type = type,
                              attributes = list(
                                title = title,
                                category = category,
                                public = public
                              )))
  } else if (is.null(description)){
    edits <- list(data = list(type = type,
                              attributes = list(
                                title = title,
                                category = category,
                                tags = tags,
                                public = public
                              )))
  } else if (is.null(tags)){edits <- list(data = list(type = type,
                                                      attributes = list(
                                                        title = title,
                                                        category = category,
                                                        description = description,
                                                        public = public
                                                      )))
  } else {
    edits <- list(data = list(type = type,
                              attributes = list(
                                title = title,
                                category = category,
                                description = description,
                                tags = tags,
                                public = public
                              )))
  }

  call <- httr::POST(url = link,
                     body = edits, encode = "json",
                     httr::authenticate(user, password))

  if (!call$status_code == 201){
    stop(sprintf("Creation of new %s failed", category))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))
  return(res)
}

put.nodes <- function(id = NULL,
                      user = NULL,
                      password = NULL,
                      type = 'nodes',
                      title = NULL,
                      description = NULL,
                      category = 'project',
                      tags = NULL,
                      public = 'true'){
  if(is.null(id)){
    stop("Requires id")}
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

  link <- construct.link(paste0('nodes/', id, '/'))

  edits <- list(data = list(type = type,
                            id = id,
                            attributes = list(
                              title = title,
                              description = description,
                              category = category,
                              tags = tags,
                              public = public
                            )))

  call <- httr::PUT(url = link,
                    body = edits, encode = "json",
                    httr::authenticate(user, password))

  if (!call$status_code == 200){
    stop(sprintf("Update of node %s failed", id))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))
  return(res)
}

patch.nodes <- function(id = NULL,
                        user = NULL,
                        password = NULL,
                        type = 'nodes',
                        title = NULL,
                        description = NULL,
                        category = 'project',
                        tags = NULL,
                        public = 'true'){
  if(is.null(id)){
    stop("Requires id")}
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

  link <- construct.link(paste0('nodes/', id, '/'))

  edits <- list(data = list(type = type,
                            id = id,
                            attributes = list(
                              title = title,
                              description = description,
                              category = category,
                              tags = tags,
                              public = public
                            )))

  call <- httr::PATCH(url = link,
                      body = edits, encode = "json",
                      httr::authenticate(user, password))

  if (!call$status_code == 200){
    stop(sprintf("Update of node %s failed", id))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))
  return(res)

}

#' Delete a node with its id
#'
#' A function to delete a node on the Open Science Framework. This includes all
#' types of nodes (e.g., communication, hypothesis, etc.) and includes a full
#' project.
#'
#' @param id The node_id to be deleted.
#' @param user The username to log in with (temporary until OAUTH2.0).
#' @param password The password to log in with (temporary until OAUTH2.0).
#' @param recursive Boolean argument to recursively delete subnodes. Defaults to
#' FALSE for sake of preventing accidental deletion.
#' @return Boolean TRUE if deletion succeeded.

delete.nodes <- function(id = NULL, user = NULL, password = NULL, recursive = FALSE){
  if (is.null(id)){
    break('Please input node to delete')}
  if (is.null(user)){
    warning("Please input username if node is private")}
  if (is.null(password)){
    warning("Please input password if node is private")}
  link <- construct.link(paste("nodes", id, sep = "/"))

  temp <- httr::DELETE(link, httr::authenticate(user, password))

  # For using in following if clause
  test <- httr::content(temp, 'text')

  if (recursive & grepl("child components must be deleted", test)){
    id_child <- recurse.nodes(id, user, password)

    # now loop through the remainder for deletion
    for (child in id_child[1:(length(id_child) - 1)]){
      link_child <- construct.link(paste('nodes',
                                            child,
                                            sep = '/'))
      httr::DELETE(link_child, httr::authenticate(user, password))
      cat(sprintf("Deleted child node %s\n", child))
    }

    # remove parent node retry
    temp <- httr::DELETE(link, httr::authenticate(user, password))
  }

  if (!temp$status_code == 204){
    cat(sprintf('Deletion of node %s failed, errorcode %s\n',
                id, temp$status_code))
    res <- FALSE
  } else {
    cat(sprintf('Deletion of node %s succeeded\n', id))
    res <- TRUE}

  return(res)
}

get.nodes.contributors <- function(node_id = NULL,
                                   user_id = NULL,
                                   user = NULL,
                                   password = NULL){
  if(is.null(user) | is.null(password)){
    warning("No username/password, if node is private will not return results")}
  if(is.null(node_id)){
    stop("Requires node_id")}
  if(is.null(user_id)){
    stop("Requires user_id")}

  link <- construct.link(paste0('nodes/', node_id, '/contributors/', user_id, '/'))

  if(!is.null(user) & !is.null(password)){
    call <- httr::GET(link, httr::authenticate(user = user, password = password))
  } else {
    call <- httr::GET(link)
  }

  if(!call$status_code == 200){
    stop("Error in retrieving user information")
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))

  return(res)
}

#' Title
#'
#' @param id
#' @param user
#' @param password
#'
#' @return Vector of node ids, first one is always the parent entered as id
#' @export
#'
#' @examples
recurse.nodes <- function(id = NULL, user = NULL, password = NULL){
  link_child <- construct.link(paste('nodes', id, 'children', sep = '/'))

  temp_child <- rjson::fromJSON(
    httr::content(
      httr::GET(link_child, httr::authenticate(user, password)), 'text'))

  while (!length(temp_child$data) == 0){
    temp_unlist <- unlist(temp_child$data)
    sel <- names(temp_unlist) == 'id'

    id = c(id, temp_unlist[sel])
    link_child <- construct.link(paste('nodes', temp_unlist[sel], 'children', sep = '/'))

    # set temp_child to empty list for next while loop if for loop fails
    temp_child <- list()

    for (baby in link_child){
      temp_child <- c(temp_child, rjson::fromJSON(
        httr::content(
          httr::GET(baby, httr::authenticate(user, password)), 'text')))
    }
  }

  # flip the id order such that the most nested node comes first
  id <- as.character(id[length(id):1])

  return(id)
}

# Placeholder functions for making a node fully public or private, including
# subnodes
public.nodes <- function(){}
private.nodes <- function(){}
