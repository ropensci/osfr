#' Searching the OSF
#'
#' @param type Specifying what type of information to search
#' @param ... Any arguments from the search.nodes or search.users functions
#'
#' @return
#' @export
#'
#' @examples search.osf(title = 'many labs', type = 'nodes')
search.osf <- function(type = 'nodes', ... = NULL)
{
  if (type == 'nodes')
  {
    res <- search.nodes(...)
  } else if (type == 'users')
  {
    res <- search.users(...)
  } else
  {
    stop('Please specify type as "nodes" or "users".')
  }
  return(res)
}

#' Title
#'
#' @param description
#' @param public
#' @param title
#' @param category
#' @param id
#' @param tags
#' @param collection
#' @param private
#'
#' @return
#' @export
#'
#' @examples
search.nodes <- function(description = NULL,
                         public = TRUE,
                         title = NULL,
                         category = NULL,
                         id = NULL,
                         tags = NULL,
                         private = FALSE)
{
  searches <- c(sprintf('filter[description]=%s', paste(description, collapse=',')),
                sprintf('filter[public]=%s', public),
                sprintf('filter[title]=%s', paste(title, collapse=',')),
                sprintf('filter[category]=%s', paste(category, collapse=',')),
                sprintf('filter[id]=%s', paste(id, collapse=',')),
                sprintf('filter[tags]=%s', paste(tags, collapse=',')))

  search <- paste(searches, collapse = '&')

  url.osf <- construct.link(sprintf('%s/?%s',
                                    'nodes',
                                    search))
  call <- httr::GET(url.osf)
  res <- rjson::fromJSON(httr::content(call, 'text'))

  while (!is.null(res$links$`next`))
  {
    whilst <- rjson::fromJSON(
      httr::content(
        httr::GET(
          res$links$`next`),
        'text'))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    cat(paste0(res$links$`next`, '\n'))
  }

  temp <- unlist(res$data)

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

  for (i in 1:length(res$data))
  {
    url[i] <- ifelse(length(res$data[[i]]$links$html) == 0,
                     NA,
                     res$data[[i]]$links$html)
    id[i] <- ifelse(length(res$data[[i]]$id) == 0,
                    NA,
                    res$data[[i]]$id)
    date_created[i] <- ifelse(length(res$data[[i]]$attributes$date_created) == 0,
                              NA,
                              res$data[[i]]$attributes$date_created)
    date_modified[i] <- ifelse(length(res$data[[i]]$attributes$date_modified) == 0,
                               NA,
                               res$data[[i]]$attributes$date_modified)
    title[i] <- ifelse(length(res$data[[i]]$attributes$title) == 0,
                       NA,
                       res$data[[i]]$attributes$title)
    registration[i] <- ifelse(length(res$data[[i]]$attributes$tegistration) == 0,
                              NA,
                              res$data[[i]]$attributes$tegistration)
    public[i] <- ifelse(length(res$data[[i]]$attributes$public) == 0,
                        NA,
                        res$data[[i]]$attributes$public)
    category[i] <- ifelse(length(res$data[[i]]$attributes$category) == 0,
                          NA,
                          res$data[[i]]$attributes$category)
    fork[i] <- ifelse(length(res$data[[i]]$attributes$fork) == 0,
                      NA,
                      res$data[[i]]$attributes$fork)
    description[i] <- ifelse(length(res$data[[i]]$attributes$description) == 0,
                             NA,
                             res$data[[i]]$attributes$description)
  }

  res <- data.frame(url,
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
#' @return
#' @export
#'
#' @examples
search.users <- function(full_name = NULL,
                         family_name = NULL)
{
  searches <- c(sprintf('filter[full_name]=%s', paste(full_name, collapse=',')),
                sprintf('filter[family_name]=%s', paste(family_name, collapse=',')))

  search <- paste(searches, collapse = '&')

  url.osf <- construct.link(sprintf('%s/?%s',
                                    'users',
                                    search))

  call <- httr::GET(url.osf)
  res <- rjson::fromJSON(httr::content(call, 'text'))

  while (!is.null(res$links$`next`))
  {
    whilst <- rjson::fromJSON(
      httr::content(
        httr::GET(
          res$links$`next`),
        'text'))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    cat(paste0(res$links$`next`, '\n'))
  }

  temp <- unlist(res$data)

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

  for (i in 1:length(res$data))
  {
    nodes <- ifelse(length(res$data[[i]]$relationships$nodes$links$related$href) == 0,
                    NA,
                    res$data[[i]]$relationships$nodes$links$related$href)
    institutions <- ifelse(length(res$data[[i]]$relationships$institutions$links$related$href) == 0,
                           NA,
                           res$data[[i]]$relationships$institutions$links$related$href)
    link_self <- ifelse(length(res$data[[i]]$links$self) == 0,
                        NA,
                        res$data[[i]]$links$self)
    id <- ifelse(length(res$data[[i]]$id) == 0,
                 NA,
                 res$data[[i]]$id)
    profile_image <- ifelse(length(res$data[[i]]$links$profile_image) == 0,
                            NA,
                            res$data[[i]]$links$profile_image)
    family_name <- ifelse(length(res$data[[i]]$attributes$family_name) == 0,
                          NA,
                          res$data[[i]]$attributes$family_name)
    suffix <- ifelse(length(res$data[[i]]$attributes$suffix) == 0,
                     NA,
                     res$data[[i]]$attributes$suffix)
    locale <- ifelse(length(res$data[[i]]$attributes$locale) == 0,
                     NA,
                     res$data[[i]]$attributes$locale)
    date_registered <- ifelse(length(res$data[[i]]$attributes$date_registered) == 0,
                              NA,
                              res$data[[i]]$attributes$date_registered)
    middle_names <- ifelse(length(res$data[[i]]$attributes$middle_names) == 0,
                           NA,
                           res$data[[i]]$attributes$middle_names)
    given_name <- ifelse(length(res$data[[i]]$attributes$given_name) == 0,
                         NA,
                         res$data[[i]]$attributes$given_name)
    full_name <- ifelse(length(res$data[[i]]$attributes$full_name) == 0,
                        NA,
                        res$data[[i]]$attributes$full_name)
    active <- ifelse(length(res$data[[i]]$attributes$active) == 0,
                     NA,
                     res$data[[i]]$attributes$active)
    timezone <- ifelse(length(res$data[[i]]$attributes$timezone) == 0,
                       NA,
                       res$data[[i]]$attributes$timezone)
  }

  res <- data.frame(nodes,
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

