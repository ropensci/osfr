#' Create a new folder on OSF project.
#'
#' @param id Parent OSF project id (osf.io/XXXX; just XXXX) to create folder in
#' @param path Name of the folder (cannot handle recursive at the moment).
#'
#' @return Waterbutler URL
#' @export

create_folder <- function(id, path) {

  config <- get_config(TRUE)

  typ <- process_type(id, private = TRUE)
  if (typ != "nodes") {
    stop("Cannot create new folder if no node ID is specified.")
  }

  lvls <- strsplit(path, "\\/")[[1]]
  path_root <- lvls[1]
  path_sub <- strsplit(path, "\\/")[[1]][2:length(lvls)]

  # Create root folder
  url_osf <- construct_link_files(id,
                                  request = paste0("?kind=folder&name=", path_root))
  url_osf <- rm_space(url_osf)
  call <- httr::PUT(url_osf, config = config)

  if (call$status_code == 409) {
    stop("Conflict in folder naming. Folder with this name already exists.")
  } else if (call$status_code != 201) {
    stop("Unsuccessful folder creation.")
  }

    res <- process_json(call)

  invisible(res$data$links$new_folder)
}

#' Delete a folder on the OSF
#'
#' @param url Waterbutler link
#'
#' @return Boolean, deletion success?
#' @seealso \code{\link{create_folder}}
#' @export

delete_folder <- function(url) {
  config <- get_config(TRUE)

  call <- httr::DELETE(url, config = config)

  if (call$status_code != 204) {
    stop('Failed to delete folder. Be sure to specify Waterbutler link.')
  }

  return(TRUE)
}
