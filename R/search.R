#' Search OSF nodes
#'
#' @param description Search in node description
#' @param title Search node titles
#' @param id Search for node id
#' @param tags Search node tags
#' @param private Boolean, search private nodes as well (TRUE) or not (FALSE)
#' @param license not used in function
#'
#' @details The categories available are:
#' \itemize{
#' \item project
#' \item hypothesis
#' \item methods and measures
#' \item procedure
#' \item instrumentation
#' \item data
#' \item analysis
#' \item communication
#' \item other
#' }
#'
#' @return Data frame of nodes
#' @export

search_nodes <- function(
  description = NULL,
  title = NULL,
  id = NULL,
  tags = NULL,
  license = NULL,
  private = FALSE) {

  searches <- c(
    ifelse(
      is.null(description), "",
      sprintf("filter[description]=%s", paste(description, collapse = ","))),
      sprintf("filter[public]=%s", !private),
    ifelse(is.null(title), "",
      sprintf("filter[title]=%s", paste(title, collapse = ","))),
    ifelse(is.null(id), "",
      sprintf("filter[id]=%s", paste(id, collapse = ","))),
    ifelse(is.null(tags), "",
      sprintf("filter[tags]=%s", paste(tags, collapse = ","))))

  search <- paste(searches, collapse = "&")
  # Ensure spaces are correct for URL
  search <- gsub(search, pattern = "\\s", replacement = "%20", perl = TRUE)

  url_osf <- construct_link(sprintf("%s/?%s", "nodes", search))
  call <- httr::GET(url_osf)
  res <- process_json(call)

  while (!is.null(res$links$`next`)) {
    whilst <- process_json(httr::GET(res$links$`next`))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    message(paste0(res$links$`next`))
  }

  # temp <- unlist(res$data)

  url <- NULL
  id <- NULL
  date_created <- NULL
  date_modified <- NULL
  title <- NULL
  registration <- NULL
  public <- NULL
  category <- NULL
  fork <- NULL
  description <- NULL

  for (i in 1:length(res$data)) {
    url[i] <- ifelse(length(res$data[[i]]$links$html) == 0,
      NA, res$data[[i]]$links$html)
    id[i] <- ifelse(length(res$data[[i]]$id) == 0,
      NA, res$data[[i]]$id)
    date_created[i] <- ifelse(length(res$data[[i]]$attributes$date_created) == 0,
      NA, res$data[[i]]$attributes$date_created)
    date_modified[i] <- ifelse(length(res$data[[i]]$attributes$date_modified) == 0,
      NA, res$data[[i]]$attributes$date_modified)
    title[i] <- ifelse(length(res$data[[i]]$attributes$title) == 0,
      NA, res$data[[i]]$attributes$title)
    registration[i] <- ifelse(length(res$data[[i]]$attributes$tegistration) == 0,
      NA, res$data[[i]]$attributes$tegistration)
    public[i] <- ifelse(length(res$data[[i]]$attributes$public) == 0,
      NA, res$data[[i]]$attributes$public)
    category[i] <- ifelse(length(res$data[[i]]$attributes$category) == 0,
      NA, res$data[[i]]$attributes$category)
    fork[i] <- ifelse(length(res$data[[i]]$attributes$fork) == 0,
      NA, res$data[[i]]$attributes$fork)
    description[i] <- ifelse(length(res$data[[i]]$attributes$description) == 0,
      NA, res$data[[i]]$attributes$description)
  }

  res <- data.frame(
    url,
    id,
    date_created,
    date_modified,
    title,
    registration,
    public,
    category,
    fork,
    description)

  return(res)
}

#' Search OSF users
#'
#' @param full_name Search the entire name
#' @param family_name Search just the family name (full_name encompasses more)
#'
#' @return Data frame of users
#' @export
search_users <- function(full_name = NULL, family_name = NULL) {
  searches <- c(
    sprintf("filter[full_name]=%s", paste(full_name, collapse = ",")),
    sprintf("filter[family_name]=%s", paste(family_name, collapse = ",")))

  search <- paste(searches, collapse = "&")
  # Ensure spaces are correct for URL
  search <- gsub(search, pattern = "\\s", replacement = "%20", perl = TRUE)

  url_osf <- construct_link(sprintf("%s/?%s", "users", search))

  call <- httr::GET(url_osf)
  res <- process_json(call)

  while (!is.null(res$links$`next`)) {
    whilst <- process_json(httr::GET(res$links$`next`))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    message(paste0(res$links$`next`))
  }

  # temp <- unlist(res$data)

  nodes <- NULL
  institutions <- NULL
  link_self <- NULL
  id <- NULL
  profile_image <- NULL
  family_name <- NULL
  suffix <- NULL
  locale <- NULL
  date_registered <- NULL
  middle_names <- NULL
  given_name <- NULL
  full_name <- NULL
  active <- NULL
  timezone <- NULL

  for (i in 1:length(res$data)) {
    nodes <- ifelse(length(res$data[[i]]$relationships$nodes$links$related$href) == 0,
      NA, res$data[[i]]$relationships$nodes$links$related$href)
    institutions <- ifelse(length(res$data[[i]]$relationships$institutions$links$related$href) == 0,
      NA, res$data[[i]]$relationships$institutions$links$related$href)
    link_self <- ifelse(length(res$data[[i]]$links$self) == 0,
      NA, res$data[[i]]$links$self)
    id <- ifelse(length(res$data[[i]]$id) == 0,
      NA, res$data[[i]]$id)
    profile_image <- ifelse(length(res$data[[i]]$links$profile_image) == 0,
      NA, res$data[[i]]$links$profile_image)
    family_name <- ifelse(length(res$data[[i]]$attributes$family_name) == 0,
      NA, res$data[[i]]$attributes$family_name)
    suffix <- ifelse(length(res$data[[i]]$attributes$suffix) == 0,
      NA, res$data[[i]]$attributes$suffix)
    locale <- ifelse(length(res$data[[i]]$attributes$locale) == 0,
      NA, res$data[[i]]$attributes$locale)
    date_registered <- ifelse(length(res$data[[i]]$attributes$date_registered) == 0,
      NA, res$data[[i]]$attributes$date_registered)
    middle_names <- ifelse(length(res$data[[i]]$attributes$middle_names) == 0,
      NA, res$data[[i]]$attributes$middle_names)
    given_name <- ifelse(length(res$data[[i]]$attributes$given_name) == 0,
      NA, res$data[[i]]$attributes$given_name)
    full_name <- ifelse(length(res$data[[i]]$attributes$full_name) == 0,
      NA, res$data[[i]]$attributes$full_name)
    active <- ifelse(length(res$data[[i]]$attributes$active) == 0,
      NA, res$data[[i]]$attributes$active)
    timezone <- ifelse(length(res$data[[i]]$attributes$timezone) == 0,
      NA, res$data[[i]]$attributes$timezone)
  }

  res <- data.frame(
    nodes,
    institutions,
    link_self,
    id,
    profile_image,
    family_name,
    suffix,
    locale,
    date_registered,
    middle_names,
    given_name,
    full_name,
    active,
    timezone)

  return(res)
}

#' Searching the OSF
#'
#' @param type Specifying what type of information to search
#' @param \ldots additional parameters passed on to \code{\link{search_nodes}} or \code{\link{search_users}}
#'
#' @return Data frame of nodes or users
#' @export
#'
#' @examples \dontrun{search.osf(title = 'many labs', type = 'nodes')}
search_osf <- function(type = "nodes", ...) {
  if (type == "nodes") {
    res <- search_nodes(...)
  } else if (type == "users") {
    res <- search_users(...)
  } else {
    stop("Please specify type as \"nodes\" or \"users\".")
  }
  return(res)
}
