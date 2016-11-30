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
#' \dontrun{get.nodes()
#' get.nodes(id = 'me')
#' get.nodes(id = 'nu97z')}

get.nodes <- function(id = NULL, # make this loopable?
                      contributors = FALSE,
                      files = FALSE,
                      children = FALSE){ # add a recurse argument?b
  if (is.null(id)){
    call <- httr::GET(construct_link('nodes'))
  } else if (id == 'me'){
    if(Sys.getenv('OSF_PAT') == '') stop('Requires login')

    # Does this work? Should test
    call <- httr::GET(construct_link('nodes'),
                      httr::add_headers(Authorization = sprintf('Bearer %s', login())))
  } else {
    call <- httr::GET(construct_link(paste('nodes', id, sep = '/')))
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
