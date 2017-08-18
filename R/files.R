#' Upload a file to the OSF (both new and revised)
#'
#' @param id OSF id (osf.io/XXXX) to upload to. Specify project to upload new file,
#'  specify a file to upload a revision
#' @param path Path to file on local machine to upload.
#' @param dest Name of the destination file on OSF (if \code{NULL}, \code{basename(path)} will be used). Note that this can be used to specify what folder to place files in, e.g. "my_folder/file.png". Also note that if \code{id} is a file ID, this is not necessary.
#'
#' @return Boolean of upload success
#' @export
#'
#' @examples
#' \dontrun{
#' upload_file(id = '12345', path = 'test.pdf')
#' }
upload_file <- function(id, path, dest = NULL) {
  type <- process_type(id, private = TRUE)

  if (is.null(dest))
    dest <- basename(path)

  if (type == "nodes") {
    fi <- get_file_info(id, private = TRUE)
    idx <- which(fi$materialized == pre_slash(dest))
    if (length(idx) != 1) {
      message("Creating new file on OSF...")
      upload_new(id, path, dest)
    } else {
      message("Revising file on OSF...")
      upload_revision(id, path, dest, fi)
    }
  } else if (type == "files") {
    message("Revising file...")
    upload_revision(id, path, dest)
  } else {
    stop("Something odd happened.\n
  If the problem persists, consider issuing a bug report on
  github.com/chartgerink/osfr")
  }
}

#' Zip up a directory and upload the zip to the OSF (both new and revised)
#'
#' @param id OSF id (osf.io/XXXX) to upload to. Specify project to upload new file,
#'  specify a file to upload a revision
#' @param path Path to directory on local machine to zip up and upload.
#' @param dest Name of the destination file on OSF (if \code{NULL}, \code{basename(path)} with a ".zip" suffix will be used). Note that this can be used to specify what folder to place files in, e.g. "my_folder/my_directory.zip". Also note that if \code{id} is a file ID, this is not necessary.
#'
#' @return Boolean of upload success
#' @export
#' @importFrom utils zip
#'
#' @examples
#' \dontrun{
#' upload_zip(id = '12345', path = 'my_dir')
#' }
upload_zip <- function(id, path, dest = NULL) {

  if (!dir.exists(path))
    stop("Please specify a valid directory to zip.")

  zp <- tempfile(fileext = ".zip")

  message("Zipping to ", zp, "...")
  zip(zp, path)

  if (is.null(dest))
    dest <- paste0(basename(normalizePath(path)), ".zip")

  if (!grepl("\\.zip$", dest))
    dest <- paste0(dest, ".zip")

  upload_file(id, zp, dest)
}

# Upload a new file to the OSF.
#
# @param id Parent OSF project id (osf.io/XXXX) to upload to.
# @param path Path to file on local machine to upload.
# @param dest Name of the destination file on OSF (if \code{NULL}, \code{basename(path)} will be used). Note that this can be used to specify what folder to place files in, e.g. "my_folder/file.png".
#
# @return Waterbutler URL
# @seealso \code{\link{upload_file}}, \code{\link{upload_revision}}
upload_new <- function(id, path, dest = NULL) {

  if (!file.exists(path))
    stop("File ", path, " does not exist on local machine.")

  if (is.null(dest))
    dest <- basename(path)

  config <- get_config(TRUE)

  # Assume it is private just in case
  # Incorporates login check needed anyway
  typ <- process_type(id, private = TRUE)
  if (typ != "nodes")
    stop("Cannot upload new file if no node ID is specified.")

  # Handle the case of file nested in folder(s)
  if (length(strsplit(dest, "\\/")[[1]]) > 1) {
    fi <- get_file_info(id, private = TRUE)
    dnm <- paste0(dirname(dest), "/")
    dnm <- pre_slash(dnm)
    idx <- which(fi$materialized == dnm)
    if (length(idx) != 1 && dirname(dnm) != "/")
      stop("Cannot create file ", dest, " because parent directory ",
        dirname(dest), " doesn't exist on server.")

    url_osf <- paste0(fi$href[idx], "?kind=file&name=", basename(dest))
  } else {
    url_osf <- construct_link_files(id,
      request = paste0("?kind=file&name=", dest))
  }

  url_osf <- gsub(url_osf, pattern = "\\s", replacement = "%20", perl = TRUE)

  call <- httr::PUT(url_osf, body = httr::upload_file(path),
    encode = "raw", config = config)

  if (call$status_code == 409)
    stop("Conflict in path naming. Please use upload_revision or change path")
  if (call$status_code != 201)
    stop("Unsuccessful upload.")

  res <- process_json(call)

  return(res$data$links$download)
}

# Upload a revised file to the OSF
#
# @param id OSF id (osf.io/XXXX) of file to revise
# @param path Path to file on local machine to upload.
# @param dest Name of the destination file on OSF to be revised (if \code{NULL}, \code{basename(path)} will be used). Note that this can be used to specify what folder to place files in, e.g. "my_folder/file.png". Also note that if \code{id} is a file ID, this is not necessary
#
# @return Boolean of revision success
# @seealso \code{\link{upload_file}}, \code{\link{upload_new}}
upload_revision <- function(id, path, dest = NULL, fi = NULL) {
  config <- get_config(TRUE)
  typ <- process_type(id, private = TRUE)

  upload_osf <- ""
  if (typ == "nodes") {
    if (is.null(dest))
      stop("Must specify a path when revising a file using a project ID.")
    if (is.null(fi))
      fi <- get_file_info(id, private = TRUE)
    dest <- pre_slash(dest)
    idx <- which(fi$materialized == dest)
    if (length(idx) != 1)
      stop("Could not find file '", dest,
        "'on OSF. Please use get_file_info(\"", id,
        "\") to see what files are available to delete")
    upload_osf <- paste0(fi$href[idx], "?kind=file")
  } else if (typ == "files") {
    url_osf <- construct_link(paste(typ, id, sep = "/"))
    call <- httr::GET(url_osf, config)
    res <- process_json(call)
    upload_osf <- res$data$links$upload
  }

  upload <- httr::PUT(
    upload_osf,
    body = httr::upload_file(path),
    encode = "raw",
    config = config)

  if (upload$status_code != 200)
    stop("Failed to upload revision")

  res <- process_json(upload)

  return(res$data$links$download)
}

#' Delete a file based on OSF file id or a project id + path
#'
#' @param id OSF id (osf.io/XXXX) for either a file or a project
#' @param path if \code{id} is a project ID, the path to the file must be specified as well
#'
#' @return Boolean, delete succeeded?
#' @export
delete_file <- function(id, path = NULL) {
  config <- get_config(TRUE)

  if (is_valid_osf_id(id) && is.null(path)) {
    url_osf <- process_file_id(id)
    if (is.null(url_osf))
      stop("Could not find a file with associated id '", id, "'.")
    call <- httr::DELETE(url = url_osf, config)
  } else {
    if (is.null(path))
      stop("Must specify a path when deleting a file using a project ID.")
    fi <- get_file_info(id, private = TRUE)
    path <- pre_slash(path)
    idx <- which(fi$materialized == path)
    if (length(idx) != 1)
      stop("Could not find file '", path,
        "' on OSF. Please use get_file_info(\"", id,
        "\") to see what files are available to delete")
    call <- httr::DELETE(url = fi$href[idx], config)
  }

  if (call$status_code != 204)
    stop("Failed to delete file ", id, ". Is it surely a file?", call. = FALSE)

  invisible(TRUE)
}

#' Move (and copy) files on the OSF
#'
#' @param from OSF file id to move (osf.io/xxxx)
#' @param to OSF id to move to (osf.io/xxxx; needs to be component)
#' @param filename Optional, rename the file
#' @param action Move or copy
#' @param conflict Keep old file or replace in case of conflict
#'
#' @return Boolean, moving succeeded?
#' @export
move_file <- function(
  from = NULL,
  to = NULL,
  filename = NULL,
  action = "move",
  conflict = "replace") {

  config <- get_config(TRUE)

  if (nchar(from) == 5) {
    typfrom <- process_type(id = from)
    typto <- process_type(id = to)

    if (typfrom != "nodes" && typto != "nodes")
      stop("File needs to move from node to node")

    url_osf <- process_file_id(from)
  } else {
    typto <- process_type(id = to)

    if (typto != "nodes")
      stop("File needs to move from node to node")

    url_osf <- from
  }

  body <- list(
    action = action,
    path = "/",
    rename = filename,
    conflict = conflict,
    provider = "osfstorage",
    resource = to)

  call <- httr::POST(
    url_osf,
    body = body, encode = "json",
    config)

  if (call$status_code != 201 && call$status_code != 200)
    stop("Error in moving/copying file '", from, "' to component '", to, "'.")

  return(TRUE)
}

#' Download files from the OSF
#'
#' @param id Specify the node id (osf.io/XXXX)
#' @param path Specify path to save file to. If NULL, defaults to OSF filename in \code{\link{tempdir}}
#' @param private Boolean to specify whether file is private
#'
#' @return Return filepath for easy processing
#' @examples
#' \dontrun{
#' download_file('zevw2', 'test123.md')
#' }
#' @importFrom utils tail
#' @export
download_file <- function(id, path = NULL, private = FALSE) {
  config <- get_config(private)

  url_osf <- construct_link(paste0("guids/", id))

  call <- httr::GET(url_osf, config)

  if (!call$status_code == 200)
    stop("Failed. Are you sure you have access to the file?")

  res <- process_json(call)

  # Find the filename as on the OSF
  if (is.null(path)) {
    txt <- res$data$attributes$name
    start <- utils::tail(gregexpr("/", txt)[[1]], 1)
    end <-  nchar(txt)
    file <- substr(txt, start + 1, end)
  } else {
  file <- paste0(path, res$data$attributes$name)
}

  message(paste0("Saving to filename: ", file))

  call <- httr::GET(res$data$links$download, config,
    httr::write_disk(file, overwrite = TRUE))

  if (call$status_code != 200)
    stop("Failed to download file.")

  message("Successfully downloaded file.")

  return(file)
}

#' Processing a file id to waterbutler
#'
#' @param id OSF id (osf.io/xxxx)
#' @param private Boolean, if file is private
#'
#' @return Waterbutler URL
process_file_id <- function(id = NULL, private = FALSE) {

  config <- get_config(private)

  url_osf <- construct_link(paste0("files/", id))
  call <- httr::GET(url = url_osf, config)
  res <- process_json(call)

  return(res$data$links$download)
}

#' Get data frame of information about all files in an OSF node
#'
#' @param id OSF id (osf.io/XXXX) for the node (project or component) to get file info for
#' @param private Boolean to specify whether to get info for private files
#'
#' @export
get_file_info <- function(id, private = FALSE) {

  config <- get_config(private)

  fix_null <- function(a) ifelse(is.null(a), NA, a)

  process_files <- function(files) {
    do.call(rbind, lapply(files, function(x) {
      data.frame(
        name = fix_null(x$attributes$name),
        materialized = fix_null(x$attributes$materialized),
        kind = fix_null(x$attributes$kind),
        guid = fix_null(x$attributes$guid), # this isn't populated until it's viewed in OSF
        resource = fix_null(x$attributes$resource),
        provider = fix_null(x$attributes$provider),
        content_type = fix_null(x$attributes$contentType), # nolint
        created_utc = fix_null(x$attributes$created_utc),
        modified_utc = fix_null(x$attributes$modified_utc),
        downloads = fix_null(x$attributes$extra$downloads),
        version = fix_null(x$attributes$extra$version),
        href = fix_null(x$links$move),
        processed = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  }

  url_osf <- construct_link_files(id = id, request = "?meta=")
  call <- httr::GET(url_osf, config)
  files <- process_json(call)$data
  files <- process_files(files)

  if (is.null(files))
    return(NULL)

  while (TRUE) {
    idx <- which(files$kind == "folder" & !files$processed)
    if (length(idx) == 0)
      break;
    res <- lapply(files$href[idx], function(href) {
      href <- paste0(href, "?kind=file&meta=")
      call <- httr::GET(href, config)
      tmp <- process_json(call)$data
      process_files(tmp)
    })
    files$processed <- TRUE
    files <- do.call(rbind, c(list(files), res))
  }

  files$processed <- NULL

  files
}
