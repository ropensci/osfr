#' Retrieve nodes associated with id
#'
#' @param id The id to search for. Use `NULL` to retrieve all,
#' `me` for logged in account. Maximum of 1 id.
#' @param contributors Boolean to extract the contributors of the node
#' @param files Boolean to download files
#' @param children Boolean to retrieve children nodes of id
#'
#' @return List object of results
#' @export
#'
#' @examples
#' get.nodes()
#' get.nodes(id = 'me')
#' get.nodes(id = 'nu97z')

get.nodes <- function(id = NULL, # make this loopable?
                      contributors = FALSE,
                      files = FALSE,
                      children = FALSE){ # add a recurse argument?b
  if (is.null(id)){
    call <- httr::GET(construct.link('nodes'))
  } else if (id == 'me'){
    if(Sys.getenv('OSF_PAT') == '') stop('Requires login')

    # Does this work? Should test
    call <- httr::GET(construct.link('nodes'),
                      httr::add_headers(Authorization = sprintf('Bearer %s', login())))
  } else {
    call <- httr::GET(construct.link(paste('nodes', id, sep = '/')))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))

  if (names(res)[1] == 'errors' & !is.null(id)) stop('Node not found.')
  if (sum(c(contributors, files, children)) > 1){
    stop('Specify contributors OR files OR children')
  }

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

    if (is.list(x$data)) stop(sprintf('No children available for node %s', id))
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

#' Create a new project
#'
#' @param user String, login username
#' @param password String, login password
#' @param type Redundant argument, required by API only.
#' @param title String of project title
#' @param description String, description of project (optional)
#' @param category String, type of node, options under details
#' @param tags Vector of strings, tags to be posted on project
#' @param public Boolean, sets project to public or private; defaults to public
#'
#' @return
#' @export
#'
#' @examples
post.nodes <- function(user = NULL,
                       password = NULL,
                       type = 'nodes',
                       title = NULL,
                       description = NULL,
                       category = 'project',
                       tags = NULL,
                       public = TRUE){
  if(is.null(user)){
    stop('Requires username')}
  if(is.null(password)){
    stop('Requires password')}
  if(is.null(title)){
    stop('Requires title')}
  if(!category %in% c('project',
                      'hypothesis',
                      'methods and measures',
                      'procedure',
                      'instrumentation',
                      'data',
                      'analysis',
                      'communication',
                      'other')){
    stop('Please input proper category, see documentation')}

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
                     body = edits, encode = 'json',
                     httr::add_headers(Authorization = sprintf('Bearer %s', login())))

  if (!call$status_code == 201){
    stop(sprintf('Creation of new %s failed', category))
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
                      public = TRUE){
  if(is.null(id)){
    stop('Requires id')}
  if(is.null(user)){
    stop('Requires username')}
  if(is.null(password)){
    stop('Requires password')}
  if(is.null(title)){
    stop('Requires title')}
  if(!category %in% c('project',
                      'hypothesis',
                      'methods and measures',
                      'procedure',
                      'instrumentation',
                      'data',
                      'analysis',
                      'communication',
                      'other')){
    stop('Please input proper category, see documentation')}

  link <- construct.link(paste0('nodes/', id, '/'))

  if (is.null(tags)){
    edits <- list(data = list(type = type,
                              attributes = list(
                                id = id,
                                title = title,
                                category = category,
                                description = description,
                                public = public
                              )))
  } else {
    edits <- list(data = list(type = type,
                              attributes = list(
                                id = id,
                                title = title,
                                category = category,
                                description = description,
                                tags = tags,
                                public = public
                              )))
  }

  call <- httr::PUT(url = link,
                    body = edits, encode = 'json',
                    httr::add_headers(Authorization = sprintf('Bearer %s', login())))

  if (!call$status_code == 200){
    stop(sprintf('Update of node %s failed', id))
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
    stop('Requires id')}
  if(is.null(user)){
    stop('Requires username')}
  if(is.null(password)){
    stop('Requires password')}
  if(is.null(title)){
    stop('Requires title')}
  if(!category %in% c('project',
                      'hypothesis',
                      'methods and measures',
                      'procedure',
                      'instrumentation',
                      'data',
                      'analysis',
                      'communication',
                      'other')){
    stop('Please input proper category, see documentation')}

  link <- construct.link(paste0('nodes/', id, '/'))

  if (is.null(tags)){
    edits <- list(data = list(type = type,
                              attributes = list(
                                id = id,
                                title = title,
                                category = category,
                                description = description,
                                public = public
                              )))
  } else {
    edits <- list(data = list(type = type,
                              attributes = list(
                                id = id,
                                title = title,
                                category = category,
                                description = description,
                                tags = tags,
                                public = public
                              )))
  }
  call <- httr::PATCH(url = link,
                      body = edits, encode = 'json',
                      httr::add_headers(Authorization = sprintf('Bearer %s', login())))

  if (!call$status_code == 200){
    stop(sprintf('Update of node %s failed', id))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))
  return(res)

}

#' Delete a node with its id
#'
#' A function to delete a node on the Open Science Framework. This includes all
#' types of nodes (e.g., communication, hypothesis, etc.) and includes a full
#' project. Note that the function recurses throughout subnodes and deletes these as well.
#'
#' @param id The node_id to be deleted.
#' @param recursive Boolean argument to recursively delete subnodes. Defaults to
#' FALSE for sake of preventing accidental deletion.
#' @return Boolean TRUE if deletion succeeded.

delete.nodes <- function(id = NULL, recursive = FALSE){
  if (is.null(id)) stop('Please input node to delete')
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login')

  link <- construct.link(paste('nodes', id, sep = '/'))

  temp <- httr::DELETE(link,
                       httr::add_headers(Authorization = sprintf('Bearer %s', login())))

  # For using in following if clause
  test <- httr::content(temp, 'text')

  if (recursive & grepl('child components must be deleted', test)){
    id_child <- recurse.nodes(id)

    # now loop through the remainder for deletion
    for (child in id_child[1:(length(id_child) - 1)]){
      link_child <- construct.link(paste('nodes',
                                         child,
                                         sep = '/'))
      httr::DELETE(link_child,
                   httr::add_headers(Authorization = sprintf('Bearer %s', login())))
      cat(sprintf('Deleted child node %s\n', child))
    }

    # remove parent node retry
    temp <- httr::DELETE(link,
                         httr::add_headers(Authorization = sprintf('Bearer %s', login())))
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

#' Function to crawl node for all sub-ids.
#'
#' @param id Parent node to crawl.
#' @param login Boolean, indicates whether crawl should occur with login or without.
#'
#' @return Vector of node ids, first one is always the parent entered as id

recurse.nodes <- function(id = NULL,
                          login = FALSE){
  link_child <- construct.link(paste('nodes', id, 'children', sep = '/'))

  if (login == FALSE) {
    temp_child <- rjson::fromJSON(
      httr::content(
        httr::GET(link_child),
                  'text'))
  } else {
    if(Sys.getenv('OSF_PAT') == '') stop('Requires login')

    temp_child <- rjson::fromJSON(
      httr::content(
        httr::GET(link_child,
                  httr::add_headers(Authorization = sprintf('Bearer %s', login()))),
                  'text'))
  }

  while (!length(temp_child$data) == 0) {
    temp_unlist <- unlist(temp_child$data)
    sel <- names(temp_unlist) == 'id'

    id = c(id, temp_unlist[sel])
    link_child <- construct.link(paste('nodes', temp_unlist[sel], 'children', sep = '/'))

    # set temp_child to empty list for next while loop if for loop fails
    temp_child <- list()

    for (baby in link_child){
      if (login == FALSE) {
        temp_child <- c(temp_child, rjson::fromJSON(
          httr::content(
            httr::GET(baby),
                      'text')))
      } else {
        if(Sys.getenv('OSF_PAT') == '') stop('Requires login')

        temp_child <- c(temp_child, rjson::fromJSON(
          httr::content(
            httr::GET(baby,
                      httr::add_headers(Authorization = sprintf('Bearer %s', login()))),
                      'text')))
      }
    }
  }

  # flip the id order such that the most nested node comes first
  id <- as.character(id[length(id):1])

  return(id)
}

#' Make node and all subnodes public
#'
#' @param id node to crawl and make public, including subnodes
#'
#' @return
#' @export

public.nodes <- function(id = NULL){
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login')

  temp <- recurse.nodes(id, login = TRUE)

  for (id in temp){
    link <- construct.link(paste0('nodes/', id, '/'))

    x <- httr::GET(link,
                   httr::add_headers(Authorization = sprintf('Bearer %s', login())))
    x <- rjson::fromJSON(httr::content(x, 'text'))

    edits <- list(data = list(type = type,
                              attributes = list(
                                id = id,
                                title = x$data$attributes$title,
                                category = x$data$attributes$category,
                                public = TRUE
                              )))

    call <- httr::PUT(url = link,
                      body = edits, encode = 'json',
                      httr::add_headers(Authorization = sprintf('Bearer %s', login())))

    if(!call$status_code == 200){
      stop('Error in making node %s public', id)
    }
  }

  return(TRUE)
}

#' Make node and all subnodes private
#'
#' @param id node to crawl and make private, including subnodes
#'
#' @return
#' @export

private.nodes <- function(id = NULL){
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login')

  temp <- recurse.nodes(id, login = TRUE)

  for (id in temp){
    link <- construct.link(paste0('nodes/', id, '/'))

    x <- httr::GET(link,
                   httr::add_headers(Authorization = sprintf('Bearer %s', login())))
    x <- rjson::fromJSON(httr::content(x, 'text'))

    edits <- list(data = list(type = type,
                              attributes = list(
                                id = id,
                                title = x$data$attributes$title,
                                category = x$data$attributes$category,
                                public = FALSE
                              )))

    call <- httr::PUT(url = link,
                      body = edits, encode = 'json',
                      httr::add_headers(Authorization = sprintf('Bearer %s', login())))

    if(!call$status_code == 200){
      stop('Error in making node %s private', id)
    }
  }

  return(TRUE)
}
