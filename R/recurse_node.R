#' Function to crawl through OSF project
#'
#' @param id OSF parent ID (osf.io/xxxx) to crawl
#' @param public Boolean, search for only public?
#' @param maxdepth Integer, amount of levels deep to crawl
#'
#' @return List of OSF ids, with parents as very last.
recurse_node <- function(
  id = NULL,
  public = TRUE,
  maxdepth = 5,
  ...) {

  url_osf <- construct_link(sprintf("nodes/%s/children", id), ...)
  call <- httr::GET(url_osf)
  res <- process_json(call)

  sel <- unlist(res)
  sel <- sel[names(sel) == "data.id"]

  i <- 1
  tmp <- sel

  while (!length(res$data) == 0 & i <= maxdepth) {
    for (child.id in tmp) {
      url_osf <- construct_link(sprintf("nodes/%s/children", child.id), ...)

      child.call <- httr::GET(url_osf)
      child.res <- process_json(child.call)

      child.sel <- unlist(child.res)
      child.sel <- child.sel[names(child.sel) == "data.id"]

      sel <- append(sel, child.sel)
    }

    i <- i + 1
  }

  # flip the id order such that the most nested node comes first
  sel <- c(as.character(sel[length(sel):1]), id)

  sel <- sel[!is.na(sel)]

  return(unique(sel))
}
