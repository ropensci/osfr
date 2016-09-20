upload.osf <- function(id = NULL,
                       ...)
{
  if (Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')
  if (is.null(id)) stop('Input component to upload to.')
  # Check what id property is, stop if NOT project/component

  url.osf <- construct.link(sprintf('nodes/%s', id), ...)

  call <- httr::GET(url.osf)

  if (call$status_code != 200)
  {
    stop('Not a proper node or something else. Check your id, pretty please.')
  }

  res <- rjson::fromJSON(httr::content(call, "text", encoding = 'UTF8'))

  httr::PUT(res$data$relationships$files$links$related$href, )
  res <- rjson::fromJSON(httr::content(call, "text", encoding = 'UTF8'))

  return(call)
}


#' Comment on a project on the OSF
#'
#' @param id OSF id as depicted in link by XXXX, osf.io/XXXX
#' @param txt Contents of the comment
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

comment.osf <- function(id = NULL,
                        txt = NULL,
                        ...)
{
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')
  if(is.null(txt)) stop('Empty comment? Seems redundant. Use the txt argument')
  if(is.null(id)) stop('Enter id to post comment to (osf.io/XXXX)')

  url.osf <- construct.link(sprintf('nodes/%s/comments/', id), ...)

  # Create the JSON body
  comment <- list(
    data = list(
      type = "comments",
      attributes = list(
        content = txt
      ),
      relationships = list(
        target = list(
          data = list(
            type = 'nodes',
            id = id
          )
        )
      )
    )
  )

  call <- httr::POST(url = url.osf,
                     body = comment, encode = 'json',
                     httr::add_headers(Authorization = sprintf(
                       'Bearer %s',
                       login())))

  if (!call$status_code == 201)
  {
    stop(sprintf('Posting comment to node %s failed.', id))
  }

  if (call$status_code == 201)
  {
    cat(sprintf('Comment posted!', id))
  }

  return(call)
}
