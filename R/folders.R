#' Upload a new file to the OSF.
#'
#' @param id Parent OSF project id (osf.io/XXXX) to upload to.
#' @param path Path of the folder to create on OSF.
#'
#' @return Waterbutler URL
#' @export
create_folder <- function(id, path) {

  config <- get_config(TRUE)

  # Assume it is private just in case
  # Incorporates login check needed anyway
  typ <- process_type(id, private = TRUE)
  if (typ != "nodes")
    stop("Cannot upload new file if no node ID is specified.")

  # Handle the case of subfolders
  if (length(strsplit(path, "\\/")[[1]]) > 1) {
    fi <- get_files_info(id)
    dnm <- paste0(dirname(path), "/")
    if (!substr(dnm, 1, 1) == "/")
      dnm <- paste0("/", dnm)
    idx <- which(fi$materialized == dnm)
    if (length(idx) != 1 && dirname(dnm) != "/")
      stop("Cannot create subdirectory '", path, "' because parent directory '",
        dirname(path), "' doesn't exist on server.")

    url_osf <- paste0(fi$href[idx], "?kind=folder&name=", basename(path))
  } else {
    url_osf <- construct_link_files(id,
      request = paste0("?kind=folder&name=", path))
  }

  url_osf <- gsub(url_osf, pattern = "\\s", replacement = "%20", perl = TRUE)
  call <- httr::PUT(url_osf, config = config)

  if (call$status_code == 409)
    stop("Conflict in folder naming. Folder with this name already exists.")
  if (call$status_code != 201)
    stop("Unsuccessful folder creation.")

  res <- process_json(call)

  invisible(res$data$links$new_folder)
}

# delete_folder <- function(id, path, recursive = FALSE) {
# }

# upload_directory <- function() {
# }
