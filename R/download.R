#' Download files from the OSF
#'
#' @param id Specify the node id (osf.io/XXXX)
#' @param file Specify path to save file
#' @param login Specify whether access requires a login (private files)
#'
#' @return
#' @export
#'
#' @examples download.osf('zevw2', 'test123.md')
download.osf <- function(id = NULL,
                         file = NULL,
                         login = FALSE){
  if(is.null(id)) stop('Enter node to download.')
  if(is.null(file)) stop('Enter filename.')

  url.osf <- sprintf('https://osf.io/%s/?action=download', id)

  if (login == TRUE)
  {
    if(Sys.getenv('OSF_PAT') == '') stop('Requires login, use login()')

    httr::GET(url.osf,
              httr::add_headers(Authorization = sprintf(
                'Bearer %s',
                login())),
              httr::write_disk(file, overwrite = TRUE))


  } else
  {
    httr::GET(url.osf,
              httr::write_disk(file, overwrite = TRUE))
  }
}
