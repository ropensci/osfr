#' Identify type of endpoint for id
#'
#' @param id OSF id to check
#' @param private Boolean, in case id is private set to TRUE
#' @param \ldots Additional parameters passed to \code{\link{construct_link}}
#'
#' @return Endpoint of id as character (nodes | files)
process_type <- function(id = NULL, private = FALSE, ...) {

  if (is.null(id))
    stop("Enter id to check.")

  url_osf_nodes <- construct_link(sprintf("nodes/%s", id), ...)
  url_osf_files <- construct_link(sprintf("files/%s", id), ...)

  if (private == TRUE) {
    if (Sys.getenv("OSF_PAT") == "")
      stop("Requires login, use login()")

    call_nodes <- httr::GET(
      url_osf_nodes,
      httr::add_headers(Authorization = sprintf("Bearer %s", login())))
    call_files <- httr::GET(
      url_osf_files,
      httr::add_headers(Authorization = sprintf("Bearer %s", login())))
  } else {
    call_nodes <- httr::GET(url_osf_nodes)
    call_files <- httr::GET(url_osf_files)
  }

  if (!call_nodes$status_code == 200 & !call_files$status_code){
    stop("Failed. Sure you have access to the id or that it is valid?")
  } else if (call_nodes$status_code == 200) {
    res <- process_json(call_nodes)
  } else {
    res <- process_json(call_files)
  }

  return(res$data$type)
}
