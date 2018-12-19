
#' @export
osf_user_retrieve <- function(id = "me") {
  out <- .osf_user_retrieve(id)
  raise_error(out)
  as_osf_tbl_user(out['data'])
}


#' Return information on an OSF user
#'
#' @param id String, OSF id (defaults to 'me'; logged in account)
#' @param nodes Boolean, return nodes available for that user?
#'
#' @return List object with account information
#' @export

get_users <- function(id = 'me', nodes = FALSE) {
  if (Sys.getenv('OSF_PAT') == '' && is.null(id)) {
    call <- httr::GET(construct_link("users"))
    res <- process_json(call)
  } else if (id == 'me'){
    if (Sys.getenv("OSF_PAT") == "") {
      warning("Please login first using the login() function")
    }

    config <- get_config(TRUE)
    url <- ifelse(nodes == TRUE, 'users/me/nodes', 'users/me')
    call <- httr::GET(construct_link(url), config)
    call <- httr::GET(construct_link(url), config)

    res <- process_json(call)
  } else {
    if (nodes == TRUE){
      call <- httr::GET(construct_link(paste0("users/?filter[id]=", id, "/nodes")))
    } else {
      call <- httr::GET(construct_link(paste0("users/?filter[id]=", id)))
    }

    res <- process_json(call)
  }

  while (!is.null(res$links$`next`)){
    whilst <- process_json(httr::GET(res$links$`next`))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    message(paste0(res$links$`next`))
  }

  return(res)
}
