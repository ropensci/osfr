#' Retrieve nodes viewable to the authenticated account on the OSF.

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
    raw <- GET(construct.link("nodes"))

    result <- fromJSON(content(raw, 'text'))$data
  } else {
    raw <- GET(construct.link(paste("nodes", id, sep = '/')))

    result <- fromJSON(content(raw, 'text'))$data
  }

  return(result)
}

post.nodes <- function(){}

put.nodes <- function(){}

patch.nodes <- function(){}

#' Delete a node with its id
#'
#' @param id The node_id to be deleted.
#' @return 1

delete.nodes <- function(id){
  link <- construct.link(paste("nodes", id, sep = "/"))

  DELETE(link)
}
