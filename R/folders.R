#' Create a new folder in an OSF project.
#'
#' Creates a root folder or a root folder and the nested subfolders. Will also
#' create subfolders in a previously created root folder.
#'
#' @param id Parent OSF project id (osf.io/XXXXX; just XXXXX) to create folder in
#' @param path Name of the folder (cannot handle recursive at the moment).
#' @param return Which waterbutler URLs should be returned. Defaults to root.
#'
#' @return Waterbutler URL for folder "root", last subfolder "sub", or all
#' folders created "all" depanding on the selection input for \code{return}
#' @export
#' @examples
#' \dontrun{
#' create_folder(id = "12345", path = "my_folder")
#' create_folder(id = "12345", path = "my_folder/my_subfolder")}

create_folder <- function(id, path, return = c("sub", "root", "all")[2]) {

  config <- get_config(TRUE)

  typ <- process_type(id)
  if (typ != "nodes") {
    stop("Cannot create new folder if no node ID is specified.")
  }

  lvls <- strsplit(path, "\\/")[[1]]
  path_root <- lvls[1]
  path_sub <- NULL
  if (length(lvls) > 1) path_sub <- strsplit(path, "\\/")[[1]][2:length(lvls)]

  # Create root folder
  url_osf <- construct_link_files(id,
                                  request = paste0("?kind=folder&name=", path_root))
  url_osf <- rm_space(url_osf)
  call <- httr::PUT(url_osf, config = config)

  if (call$status_code == 409) {
    warning("Conflict in folder naming. Root folder with this name already exists.")
  } else if (call$status_code != 201) {
    http_error(call$status_code, "Unsuccessful folder creation.")
  }

  # If root folder creation is successful, create link to the new folder.
  # If unsuccessful, check to see if the folder already exists and save link
  # for the creation of subfolders.

  if (call$status_code == 201) {
    res <- process_json(call)
    res_root_link <- res$data$links$new_folder
  } else if (call$status_code == 409) {
    message("Attempting to create subfolders under previously created root folder")
    fi <- get_files_info(id, private = TRUE)
    fi_row <- which(fi$materialized == paste0(pre_slash(path_root), "/"))
    res_root_link <- paste0(fi[fi_row, "href"],'?kind=folder')
  }

  # Create subfolders and keep the url json information for each creation.

  res_sub <- vector("list", length(path_sub))
  res_data_link <- res_root_link

  for (i in seq_along(path_sub)){

    url_osf_sub <- paste0(res_data_link, "&name=", path_sub[i])
    call_sub <- httr::PUT(url_osf_sub, config = config)

    if (call_sub$status_code == 409) {
      http_error(call_sub$status_code, "A subfolder with this name already exists.")
    } else if (call_sub$status_code != 201) {
      http_error(call_sub$status_code, "Unsuccessful subfolder creation.")
    }

    res_sub[[i]] <- process_json(call_sub)
    res_data_link <- res_sub[[i]]$data$links$new_folder

  }
  names(res_sub) = path_sub

  # Check the return parameter. If set to "all", return links to all of new
  # folders created. If set to "root", return only the link to the lowest level
  # folder created. If set to "sub", return the links of all of the subfolders
  # created.

  if(return == "all") {
    res_sub_links <- lapply(res_sub, function(x) x$data$links$new_folder)

    out <- c(res$data$links$new_folder, res_sub_links)
    names(out)[1] <- path_root
  } else if (return == "root") {
    out <- res_root_link
  } else if (return == "sub") {
    out <- res_sub[[length(res_sub)]]$data$links$new_folder
  } else {
    out <- NULL
  }

  invisible(out)

}

#' Delete a folder on OSF
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
    http_error(call$status_code,
               'Failed to delete folder. ',
               'Be sure to specify Waterbutler link.')
  }

  return(TRUE)
}
