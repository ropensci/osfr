upload_osf <- function(id = NULL,
                       ...)
{
  if (is.null(id)) stop('Input component to upload to.')

  if (check_type(id) == 'nodes')
  {
    upload_new()
  } else if (check_type(id) == 'files')
  {
    upload_revision()
  } else
  {
    stop('Something odd happened. Please report a bug with at https://github.com/chartgerink/osfr')
  }

  return(call)
}

upload_new <- function(id = NULL,
                       ...)
{
  if (Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')
}

upload_revision <- function(id = NULL,
                            revision = NULL,
                            ...)
{
  if (is.null(revision)) stop('Please input file to be uploaded as revision.')

  # Assume it is private just in case
  # Incorporates login check needed anyway
  typ <- check_type(id, private = TRUE, ...)

  if (typ != 'files') stop('Cannot upload revisions if not a file.')

  url.osf <- construct.link(sprintf('%s/%s',
                                    typ,
                                    id), ...)

  call <- httr::GET(url.osf, httr::add_headers(Authorization = sprintf(
    'Bearer %s',
    login())))
  res <- process_json(call)

  upload.osf <- res$data$links$upload

  upload <- httr::PUT(upload.osf, body = httr::upload_file(revision), encode = 'raw',
             config = httr::add_headers(Authorization = sprintf(
               'Bearer %s',
               login())))

  if (upload$status_code != 200) stop('Failed to upload revision')

  return(TRUE)
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

comment_osf <- function(id = NULL,
                        txt = NULL,
                        ...)
{
  if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')
  if(is.null(txt)) stop('Empty comment? Seems redundant. Use the txt argument')
  if(is.null(id)) stop('Enter id to post comment to (osf.io/XXXX)')

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
