#' Comment on a project on the OSF
#'
#' @param id OSF id (osf.io/XXXX)
#' @param txt Contents of the comment
#'
#' @return Boolean, posting succeeded?
#' @export
#'
#' @examples
#' \dontrun{comment(id = '12345', txt = 'This is an example')}

comment <- function(id = NULL,
                    txt = NULL,
                    ...)
{
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')
  if(is.null(txt)) stop('Empty comment? Seems redundant. Use the txt argument')
  if(is.null(id)) stop('Enter id to post comment to (osf.io/XXXX)')
  if (process_type(id, ...) != 'nodes') stop('Currently unable to post
                                             comments to files.')

  url.osf <- construct_link(sprintf('nodes/%s/comments/', id), ...)

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

  if (!call$status_code == 201) stop(sprintf('Posting comment to node %s failed.', id))

  return(TRUE)
}
