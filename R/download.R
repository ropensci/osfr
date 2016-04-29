#' Download files from the OSF
#'
#' @param id Specify the node id (osf.io/XXXX)
#' @param private Boolean to specify whether file is private
#' @param file Specify path to save file
#'
#' @return
#' @export
#'
#' @examples download.osf('zevw2', 'test123.md')
download.osf <- function(id = NULL,
                         file = NULL,
                         private = FALSE)
{
  if(is.null(id)) stop('Enter node to download.')
  if(is.null(file)) stop('Enter filename.')

  url.osf <- construct.link(sprintf('guids/%s', id))

  if (private == TRUE)
  {
    if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')

    call <- httr::GET(url.osf,
              httr::add_headers(Authorization = sprintf(
                'Bearer %s',
                login())))


  } else
  {
    call <- httr::GET(url.osf)
  }

  if (!call$status_code == 200){
    stop(sprintf('Failed. Sure it is the right node id?', category))
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))

  httr::GET(res$data$links$download,
            httr::write_disk(file, overwrite = TRUE))
}
