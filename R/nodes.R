#' Retrieve nodes associated with id
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
#' \dontrun{get_nodes()
#' get_nodes(id = 'me')
#' get_nodes(id = 'nu97z')
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
    call <- httr::GET(res$data$relationships$files$links$related$href, config)
    res <- process_json(call)
  }

  if (children) {
    call <- httr::GET(res$data$relationships$children$links$related$href, config)
    res <- process_json(call)

    if (!is.list(res$data))
      stop(sprintf("No children available for node %s", id))
  }

  while (!is.null(res$links$`next`)) {
    whilst <- rjson::fromJSON(
      httr::content(
        httr::GET(res$links$`next`, config), "text"))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    message(paste0(res$links$`next`))
  }

  return(res)
}

#' Function to crawl through OSF project
#'
#' @param id OSF parent ID (osf.io/xxxx) to crawl
#' @param private Boolean, search for private too?
#' @param maxdepth Integer, amount of levels deep to crawl
#'
#' @return List of OSF ids, with parents as very last.
recurse_node <- function(
  id = NULL,
  private = FALSE, # NOTE: not used
  maxdepth = 5) {

  config <- get_config(private)

  url_osf <- construct_link(sprintf("nodes/%s/children", id))
  call <- httr::GET(url_osf, config)
  res <- process_json(call)

  sel <- unlist(res)
  sel <- sel[names(sel) == "data.id"]

  i <- 1
  tmp <- sel

  while (!length(res$data) == 0 && i <= maxdepth) {
    for (child_id in tmp) {
      url_osf <- construct_link(sprintf("nodes/%s/children", child_id))

      child_call <- httr::GET(url_osf, config)
      child_res <- process_json(child_call)

      child_sel <- unlist(child_res)
      child_sel <- child_sel[names(child_sel) == "data.id"]

      sel <- append(sel, child_sel)
    }

    i <- i + 1
  }

  # flip the id order such that the most nested node comes first
  sel <- c(as.character(sel[length(sel):1]), id)

  sel <- sel[!is.na(sel)]

  return(unique(sel))
}
