#' Construct an API link with proper base
#'
#' @param request The request link to be combined with the base API link.
#'
#' @return The full request link with proper base
#' @examples
#' \dontrun{
#' construct_link("nodes/{node_id}/files/")
#' }
construct_link <- function(request = NULL) {
  if (Sys.getenv("OSF_USE_TEST_SERVER") != "") {
    base <- "https://test-api.osf.io/v2/"
  } else {
    base <- "https://api.osf.io/v2/"
  }

  paste0(base, request)
}

#' Construct a waterbutler API link with proper base
#'
#' @param id OSF id
#' @param request Request for waterbutler
#'
#' @return Waterbutler link
construct_link_files <- function(id = NULL, request = NULL) {
  if (Sys.getenv("OSF_USE_TEST_SERVER") != "") {
    base <- sprintf("https://test-files.osf.io/v1/resources/%s/providers/osfstorage/%s",
      id, request)
  } else {
    base <- sprintf("https://files.osf.io/v1/resources/%s/providers/osfstorage/%s",
      id, request)
  }

  return(base)
}

#' Processing whether a category is valid
#'
#' @param category Category to check for validity. Valid categories:
#' \itemize{
#'   \item project
#'   \item hypothesis
#'   \item methods and measures
#'   \item procedure
#'   \item instrumentation
#'   \item data
#'   \item analysis
#'   \item communication
#'   \item other
#' }
#'
#' @return Nothing if succeeded
#' @export
process_category <- function(category = "") {
  if (!category %in% c("project",
                      "hypothesis",
                      "methods and measures",
                      "procedure",
                      "instrumentation",
                      "data",
                      "analysis",
                      "communication",
                      "other")) {
    stop("Please input proper category, see documentation")
  }
}

#' Function to parse API call
#'
#' @param x Object containing the result of an API call.
#'
#' @return Parsed JSON object in the form of an R object.
process_json <- function(x) {
  rjson::fromJSON(httr::content(x, "text", encoding = "UTF-8"))
}

#' Identify type of endpoint for id
#'
#' @param id OSF id to check
#' @param private Boolean, in case id is private set to TRUE
#'
#' @return Endpoint of id as character (nodes | files)
process_type <- function(id = NULL, private = FALSE) {

  if (is.null(id))
    stop("Enter id to check.")

  url_osf_nodes <- construct_link(sprintf("nodes/%s", id))
  url_osf_files <- construct_link(sprintf("files/%s", id))

  config <- get_config(private)

  call_nodes <- httr::GET(url_osf_nodes, config)
  call_files <- httr::GET(url_osf_files, config)

  if (!call_nodes$status_code == 200 && !call_files$status_code) {
    stop("Failed. Are you sure you have access to the id or that it is valid?")
  } else if (call_nodes$status_code == 200) {
    res <- process_json(call_nodes)
  } else {
    res <- process_json(call_files)
  }

  if (is.null(res$data$type))
    return("")

  return(res$data$type)
}

get_config <- function(login_required) {
  config <- list()
  if (login_required) {
    config <- httr::add_headers(Authorization = sprintf("Bearer %s", login()))
    check_login()
  }
  config
}

check_login <- function() {
  if (Sys.getenv("OSF_PAT") == "")
    stop("Requires login, use login()", call. = FALSE)
}

is_valid_osf_id <- function(x) {
  grepl("[A-Za-z0-9]{5}", x)
}

pre_slash <- function(x) {
  if (!substr(x, 1, 1) == "/")
    x <- paste0("/", x)
  x
}
