download_files <- function () {}

#' Upload a new file to the OSF.
#'
#' @param id Parent OSF project id (osf.io/XXXX) to upload to.
#' @param path Path to file on local machine to upload. Ensure file has
#' proper extension named (i.e., extension sensitive, not like on Linux)
#' @param name Name of the uploaded file (if \code{NULL}, current name will be
#' used).
#' @param href_hash the element that identifies the folder hash of the href from the folder URL. Not use if \code{NULL}.
#' \code{basename(path)} will be used).
#'
#' @return Waterbutler URL
#' @seealso \code{\link{upload_files}}, \code{\link{upload_revised_files}}

upload_new_files <- function(id, path, name = NULL) {
  if (!file.exists(path)) {
    stop(sprintf('File %s does not exist on local machine.', path))
  } else if (is.null(name)) {
    name <- basename(path)
  }

  config <- get_config(TRUE)

  typ <- process_type(id, private = TRUE)
  if (typ != 'nodes') {
    stop('Cannot upload new file if no node ID is specified.')
  }
  # default `provider` is 'osfstorage'.
  url_osf <- construct_link_files(id, request = paste0(href_hash, '?kind=file&name=',
                                                       name))

  # Ensure proper spaces in URL
  url_osf <- gsub(url_osf, pattern = '\\s', replacement = '%20', perl = TRUE)

  call <- httr::PUT(url_osf, body = httr::upload_file(path),
                    encode = 'raw', config = config)

  if (call$status_code == 409) {
    stop('Conflict in path naming. Please use upload_revised_files or change path')
  } else if (call$status_code != 201) {
    stop('Unsuccessful upload.')
  }

  res <- process_json(call)

  return(res$data$links$download)
}


#' Upload a file to the OSF (both new and revised)
#'
#' @param id OSF id (osf.io/XXXX; just XXXX) to upload to. Specify project to
#' upload a new file, specify a file to upload a revision.
#' @param path Path to file on local machine to upload.
#' @param dest Name of the destination file on OSF (if \code{NULL},
#' \code{basename(path)} will be used). Note that this can be
#'  used to specify what folder to place files in, e.g. 'my_folder/file.png'.
#'  Also note that if \code{id} is a file ID, this is not necessary.
#'
#' @return Link if new file created
#' @export
#'
#' @examples
#' \dontrun{
#' upload_files(id = '12345', path = 'test.pdf')
#' }

upload_files <- function(id, path, dest = NULL) {
  type <- process_type(id, private = TRUE)
  subfolder_file <- FALSE

  if (is.null(dest)) {
    dest <- basename(path)
  }

  ## Here we need to check for 'nodes' then have another condition within nodes. see `idx_folder`
  if (type == "nodes") {

    fi <- get_files_info(id, private = TRUE)

    # Added check to see if 'fi' is NULL. If no file exists in a component, this not run.
    if (!is.null(fi)) {
      idx <- which(fi$materialized == pre_slash(dest))

      # check for destination value subfolder
      if (!is.null(dest)) {
        fi_folder <- paste0(dirname(dest), "/")
        dest_fname <- basename(dest)

        idx_folder <- which(fi$materialized == pre_slash(fi_folder))

        hash_folder <- paste0(basename(fi[idx_folder, "href"]), "/")

        # check for subfolder created and file not there.
        if (length(idx) != 1 & length(idx_folder) == 1) {
          message(paste0('Creating new file on OSF in subfolder ', fi_folder  ,' ...'))
          upload_new_files(id, path, dest_fname, href_hash = hash_folder)
          subfolder_file <- TRUE
        }
        if (length(idx_folder) != 1 & dirname(fi_folder) != ".") {
          stop("subfolders not created. Create subfolder before creating a file in the folder")
        }
      }
    }
    ### End addition

    # changed condition for new file creation

    if (length(idx) != 1 & !subfolder_file) {
      message('Creating new file on OSF...')
      upload_new_files(id, path, dest)
    } else if (!subfolder_file) {
      message('Revising file on OSF...')
      upload_revised_files(id, path, dest, fi)
    }

  } else if (type == 'files') {
    message('Revising file...')
    upload_revised_files(id, path)
  } else {
    stop('Something odd happened.\n
          If the problem persists, consider issuing a bug report on
          github.com/chartgerink/osfr')
  }
}

#' Zip up a directory and upload the zip to the OSF (both new and revised)
#'
#' @param id OSF id (osf.io/XXXX) to upload to. Specify project to upload new file,
#'  specify a file to upload a revision.
#' @param path Path to directory on local machine to zip up and upload.
#' @param dest Name of the destination file on OSF (if \code{NULL}, \code{basename(path)} with a '.zip' suffix will be used). Note that this can be used to specify what folder to place files in, e.g. 'my_folder/my_directory.zip'. Also note that if \code{id} is a file ID, this is not necessary.
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

  if (!dir.exists(path)) {
    stop('Please specify a valid directory to zip.')
  }

  zp <- tempfile(fileext = '.zip')

  message('Zipping to ', zp, '...')
  zip(zp, path)

  if (is.null(dest)) {
    dest <- paste0(basename(normalizePath(path)), '.zip')
  }

  if (!grepl('\\.zip$', dest)) {
    dest <- paste0(dest, '.zip')
  }

  upload_files(id, zp, dest)
}

#' Upload a revised file to the OSF
#'
#' @param id OSF id (osf.io/XXXX; just XXXX) of file to revise
#' @param path Path to file on local machine to upload.
#'
#' @return Boolean, revision success? (invisible)
#' @seealso \code{\link{upload_files}}, \code{\link{upload_new_files}}

upload_revised_files <- function(id, path) {

  if (!file.exists(path)) {
    stop(sprintf('File %s does not exist on local machine.', path))
  }

  config <- get_config(TRUE)
  typ <- process_type(id, private = TRUE)

  if (typ == 'nodes') {
    stop('Specify an OSF id referring to a file.')
  } else if (typ == 'files') {
    url_osf <- construct_link(paste(typ, id, sep = '/'))
    call <- httr::GET(url_osf, config)
    res <- process_json(call)
    upload_osf <- res$data$links$upload
  } else {
    stop('Unknown error occurred. Please file issue on GitHub.')
  }

  call <- httr::PUT(
    upload_osf,
    body = httr::upload_file(path),
    encode = 'raw',
    config = config)

  if (call$status_code != 200) {
    stop('Failed to upload revision')
  }

  invisible(TRUE)
}

#' Delete a file based on OSF id
#'
#' @param id OSF id (osf.io/XXXX; just XXXX)
#'
#' @return Boolean, delete succeeded?
#' @export

delete_files <- function(id) {

  config <- get_config(TRUE)

  if (!is_valid_osf_id(id)) {
    stop('Please insert valid OSF id.')
  }

  url_osf <- process_file_id(id, private = TRUE)

  if (is.null(url_osf)) {
    stop(sprintf('Could not find a file with associated id %s.', id))
  }

  call <- httr::DELETE(url = url_osf, config)

  if (call$status_code != 204) {
    stop(sprintf('Failed to delete file %s.', id), call. = FALSE)
  }

  invisible(TRUE)
}

#' Move (and copy) files on the OSF
#'
#' @param from OSF file id to move (osf.io/xxxx; just xxxx)
#' @param to OSF id to move to (osf.io/xxxx; needs to be component)
#' @param filename Optional, rename the file
#' @param action 'move' or 'copy'
#' @param conflict Keep old file or replace in case of conflict
#'
#' @return Boolean, moving succeeded? (invisible)
#' @export

move_files <- function(
  from = NULL,
  to = NULL,
  filename = NULL,
  action = 'move',
  conflict = 'replace') {

  config <- get_config(TRUE)

  if (nchar(from) == 5) {
    typfrom <- process_type(id = from)
    typto <- process_type(id = to)

    if (typfrom != 'nodes' && typto != 'nodes') {
      stop('File needs to move from node to node')
    }

    url_osf <- process_file_id(from)
  } else {
    typto <- process_type(id = to)

    if (typto != 'nodes') {
      stop('File needs to move from node to node')
    }

    url_osf <- from
  }

  body <- list(
    action = action,
    path = '/',
    rename = filename,
    conflict = conflict,
    provider = 'osfstorage',
    resource = to)

  call <- httr::POST(
    url_osf,
    body = body, encode = 'json',
    config)

  if (call$status_code != 201 && call$status_code != 200) {
    stop('Error in moving/copying file, from to component to')
  }

  invisible(TRUE)
}

#' Download files from the OSF
#'
#' This function downloads files from the OSF and assumes that the file is
#' public. For private files, the function checks first for a view-only link.
#' If no view-only link is provided, the user's login credentials are used.
#'
#' For more information on creating a view-only link see:
#' \url{http://help.osf.io/m/links/l/524049-create-a-view-only-link}.
#'
#'
#' @param id Specify the node id (osf.io/XXXX)
#' @param version Specify the OSF version id (string)
#' @param path Specify path to save file to. If NULL, defaults to OSF filename in the working directory
#' @param view_only Specify the view-only link (string)
#'
#' @return Return filepath for easy processing
#' @examples
#' \dontrun{
#' download_files('5z2bh', 'public_test_file.csv')
#' download_files('852dp', 'view_only_test_file.csv', view_only = 'https://osf.io/jy9gm/?view_only=a500051f59b14a988415f08539dbd491')
#' }
#' @importFrom utils tail
#' @export

download_files <- function(id, path = NULL, view_only = NULL, version = NULL) {
  config <- list()

  url_osf <- construct_link(paste0('guids/', id))

  call <- httr::GET(url_osf, config)

  res <- process_json(call)

  # Check if data from processed json is empty and get the file information
  # using authentication. If a view-only link is present, then the file is
  # downloaded using the view-only link. If no view-only link is present,
  # then the file is downloaded with the user's login.
  if (is.null(res$data) && !is.null(view_only)) {
    # Remove the view-only tag from the provided view-only link and paste to
    # the file url
    view_only_url <- paste0(url_osf, '/', gsub(".*/", "", view_only))

    call <- httr::GET(view_only_url, config)

    if (!call$status_code == 200) {
      stop('Failed. Are you sure you have access to the file?')
    }

    res <- process_json(call)

  } else if (is.null(res$data) && is.null(view_only)) {
    config <- get_config(TRUE)

    call <- httr::GET(url_osf, config)

    if (!call$status_code == 200) {
      stop('Failed. Are you sure you have access to the file?')
    }

    res <- process_json(call)
  }

  # Determine the file name to save the file as.
  # If no path is provided, use the OSF file name.
  # If a path is provided, determine if the path is just a folder path or if
  # the path includes a file name.
  if (is.null(path)) {
    file <- res$data$attributes$name
  } else if (grepl('/$', path)) {
    file <- paste0(path, res$data$attributes$name)
  } else {
    file <- path
  }

  message(paste0('Saving to filename: ', file))

  if (is.null(version)) {
    call <- httr::GET(res$data$links$download, config,
                      httr::write_disk(file, overwrite = TRUE))
  } else {
    call <- httr::GET(paste0(res$data$links$download, '?revision=', version), config,
                      httr::write_disk(file, overwrite = TRUE))
  }
  if (call$status_code == 404) {
    stop('Version of file does not exist.')
  } else if (call$status_code != 200) {
    stop('Failed to download file.')
  }

  message('Successfully downloaded file.')

  return(file)
}

#' Get data frame of information about all files in an OSF node
#'
#' @param id OSF id (osf.io/XXXX) for the node (project or component) to get file info for
#' @param private Boolean to specify whether to get info for private files
#'
#' @export

get_files_info <- function(id, private = FALSE) {

  config <- get_config(private)

  fix_null <- function(a) ifelse(is.null(a), NA, a)

  process_files <- function(files) {
    do.call(rbind, lapply(files, function(x) {
      data.frame(
        name = fix_null(x$attributes$name),
        materialized = fix_null(x$attributes$materialized_path),
        kind = fix_null(x$attributes$kind),
        guid = fix_null(x$attributes$guid), # this isn't populated until it's viewed in OSF
        provider = fix_null(x$attributes$provider),
        created_utc = fix_null(x$attributes$date_created),
        modified_utc = fix_null(x$attributes$date_modified),
        downloads = fix_null(x$attributes$extra$downloads),
        version = fix_null(x$attributes$current_version),
        href = fix_null(x$links$move),
        folder_link = fix_null(x$relationships$files$links$related$href),
        processed = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  }

  # Creates a link to the OSF
  url_osf <- construct_link(request = paste0('nodes/', id, '/files/osfstorage/'))

  # Calls the link
  call <- httr::GET(url_osf, config)

  # Process the requested JSON
  res <- process_json(call)

  # Process pagination
  files <- process_pagination(res, config)

  # Turn JSON into a data frame
  files <- process_files(files)

  if (is.null(files)) {
    return(NULL)
  }

  # Process all of the nested subfolders.
  # This is done by calling each of the folder links and processing through
  # the files. The loop begins with the top level folder, gets everything from
  # its subfolders, and then repeats the loop through any further nested
  # folders.
  while (TRUE) {
    idx <- which(files$kind == 'folder' & !files$processed)
    if (length(idx) == 0)
      break;
    res <- lapply(files$folder_link[idx], function(href) {
      call <- httr::GET(href, config)
      tmp <- process_json(call)
      pag_tmp <- process_pagination(tmp, config)
      process_files(pag_tmp)
    })
    files$processed <- TRUE
    files <- do.call(rbind, c(list(files), res))
  }

  files$processed <- NULL

  return(files)
}

#' Get file path for the latest version of a file (for direct reading)
#'
#' @param id Specify the file id (osf.io/XXXX)
#' @param private Boolean to specify whether file is private
#'
#' @return Return filepath (online)
#' @examples
#' \dontrun{
#' read.csv(path_file("myxcv"))
#' }
#' @importFrom utils tail
#' @export

path_file <- function(id, private = FALSE) {
  config <- get_config(private)

  typ <- process_type(id, private = TRUE)

  if (typ == 'nodes') {
    stop('Specify an OSF id referring to a file.')
  } else if (typ == 'files') {
    url_osf <- construct_link(paste0('guids/', id))
    call <- httr::GET(url_osf, config)
    res <- process_json(call)
    } else {
    stop('Unknown error occurred. Please file issue on GitHub.')
  }

  if (!call$status_code == 200) {
    stop('Failed. Are you sure you have access to the file?')
  }

  return(res$data$links$download)
}

