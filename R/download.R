#' Download files from the OSF
#'
#' @param id Specify the node id (osf.io/XXXX)
#' @param file Specify path to save file. If NULL, defaults to OSF
#' @param private Boolean to specify whether file is private
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
    stop('Failed. Sure you have access to the file?')
  }

  res <- rjson::fromJSON(httr::content(call, 'text'))

  # Find the filename as on the OSF
  if (is.null(file))
  {
    txt <- res$data$links$download
    start <- tail(gregexpr('/', txt)[[1]], 1)
    end <-  nchar(txt)
    file <- substr(txt, start + 1, end)
  }

  httr::GET(res$data$links$download,
            httr::write_disk(file, overwrite = TRUE))
}
