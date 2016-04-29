upload.osf <- function(id = NULL)
{
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')

}

#' Comment on a project on the OSF
#'
#' @param id OSF id as depicted in link by XXXX, osf.io/XXXX
#' @param txt Comment text
#'
#' @return
#' @export
#'
#' @examples
comment.osf <- function(id = NULL,
                        txt = NULL)
{
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')
  if(is.null(txt)) stop('Empty comment? Seems redundant. Use the txt argument')
  if(is.null(id)) stop('Enter id to post comment to (osf.io/XXXX)')

  url.osf <- construct.link(sprintf('nodes/%s/comments/', id))

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

  rjson::fromJSON(httr::content(call, 'text'))
}
