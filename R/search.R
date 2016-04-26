search.osf <- function(type = 'nodes', ... = NULL)
{
  if (type == 'nodes')
  {
    res <- search.nodes(...)
  } else if (type == 'users')
  {
    res <- NULL
  } else
  {
    res <- NULL
  }
   return(res)
}

search.nodes <- function(description = NULL,
                         public = TRUE,
                         title = NULL,
                         category = NULL,
                         id = NULL,
                         tags = NULL,
                         collection = FALSE,
                         private = FALSE)
{
  searches <- c(sprintf('filter[description]=%s', paste(description, collapse=',')),
                sprintf('filter[public]=%s', public),
                sprintf('filter[title]=%s', paste(title, collapse=',')),
                sprintf('filter[category]=%s', paste(category, collapse=',')),
                sprintf('filter[id]=%s', paste(id, collapse=',')),
                sprintf('filter[tags]=%s', paste(tags, collapse=',')),
                sprintf('filter[collection]=%s', collection))

  search <- paste(searches, collapse = '&')

  url.osf <- construct.link(sprintf('%s/?%s',
                                    'nodes',
                                    search))
  call <- httr::GET(url.osf)
  res <- rjson::fromJSON(httr::content(call, 'text'))

  #   while (!is.null(res$links$`next`))
  #   {
  #     whilst <- rjson::fromJSON(
  #       httr::content(
  #         httr::GET(
  #           res$links$`next`),
  #         'text'))
  #     res$data <- c(res$data, whilst$data)
  #     res$links$`next` <- whilst$links$`next`
  #     cat(paste0(res$links$`next`, '\n'))
  #   }

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
