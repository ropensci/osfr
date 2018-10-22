# Create a new node
create_node <- function(
  path,
  title,
  description = '',
  private = TRUE) {

  if (missing(title)) stop("Specify a node title")

  body <- list(
    data = list(
      type = "nodes",
      attributes = list(
        title = title,
        category = "project",
        description = description,
        public = !private
      )
    )
  )

  cli <- osf_cli()
  res <- cli$post(path, body = body, encode = "json")
  res$raise_for_status()

  jsonlite::fromJSON(res$parse("UTF-8"))
}


# Update a node
update_node <- function(
  id,
  title = NULL,
  description = NULL,
  private = NULL) {

  if (missing(id)) stop("Must specify a node identifier")
  public <- if (is.logical(private)) !private else NULL

  body <- list(
    data = list(
      type = "nodes",
      id = id,
      attributes = list()
    )
  )

  attrs <- list(title = title, description = description, public = public)
  body$data$attributes <- utils::modifyList(body$data$attributes, attrs)

  if (length(body$data$attributes) == 0) {
    stop("No updated attribute values specified")
  }

  cli <- osf_cli()
  path <- osf_path(sprintf('nodes/%s/', id))
  res <- cli$patch(path, body = body, encode = "json")
  res$raise_for_status()

  jsonlite::fromJSON(res$parse("UTF-8"))
}


#' Retrieve nodes associated with id
#'
#' This function retrieves the JSON returned by the OSF API for a given OSF id.
#' If the \code{contributors}, \code{files}, or \code{children} arguments are
#' set to \code{TRUE}, then the linked JSON for that category is returned. Note
#' that only one of \code{contributors}, \code{files}, and \code{children} can
#' be selected.
#'
#' @param id The id to search for. Use `NULL` to retrieve all,
#' `me` for logged in account. Maximum of 1 id.
#' @param contributors Boolean to extract the contributors of the node
#' @param files Boolean to retrieve files
#' @param children Boolean to retrieve children nodes of id
#' @param private Boolean, retrieve private node
#'
#' @return List object of results
#' @export
#'
#' @examples
#' \dontrun{
#' get_nodes()
#' get_nodes(id = 'me')
#' get_nodes(id = 'm5pds')
#' }

get_nodes <- function(
  id = NULL, # make this loopable?
  contributors = FALSE,
  files = FALSE,
  children = FALSE,
  private = FALSE) {
  # add a recurse argument?

  id <- ifelse(is.null(id), "me", id)

  config <- get_config(private || id == "me")

  if (id == "me") {
    call <- httr::GET(construct_link("nodes"), config)
  } else {
    call <- httr::GET(construct_link(paste("nodes", id, sep = "/")), config)
  }

  res <- process_json(call)

  if (names(res)[1] == "errors" && !is.null(id))
    stop("Node not found.")

  if (sum(c(contributors, files, children)) > 1)
    stop("Specify contributors OR files OR children")

  if (contributors) {
    call <- httr::GET(res$data$relationships$contributors$links$related$href, config)
    res <- process_json(call)
  }

  if (files) {
    # Change to access actual files id under guid tag within osfstorage
    call <- httr::GET(paste0(res$data$relationships$files$links$related$href, "/osfstorage/"), config)
    res <- process_json(call)
  }

  if (children) {
    call <- httr::GET(res$data$relationships$children$links$related$href, config)
    res <- process_json(call)

    if (!is.list(res$data))
      stop(sprintf("No children available for node %s", id))
  }

  while (!is.null(res$links$`next`)) {
    whilst <- process_json(httr::GET(res$links$`next`, config))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    message(paste0(res$links$`next`))
  }

  return(res)
}

#' Function to crawl through OSF project
#'
#' @param id OSF parent ID (osf.io/XXXXX) to crawl
#' @param maxdepth Integer, amount of levels deep to crawl
#'
#' @return Character vector of OSF IDs for children of the parent, with the
#'   parent ID listed last. If there are no child nodes to recurse, the parent
#'   ID is returned.

recurse_node <- function(id, maxdepth = 5) {
  node_tree <- recurse_tree(id, maxdepth)
  simplify_tree(
    setNames(list(node_tree), id)
  )
}


