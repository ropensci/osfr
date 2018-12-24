#' Create a OSF folder
#'
#' Creates a new folder in an OSF project or component.
#'
#' @param id Parent OSF project id (osf.io/XXXXX; just XXXXX) to create folder in
#' @param path Name for the new folder
#' @param parent_id Target path for the new folder
#' @template verbose
#'
#' @return Waterbutler URL for folder "root", last subfolder "sub", or all
#' folders created "all" depanding on the selection input for \code{return}
#' @export
#' @examples
#' \dontrun{
#' proj <- osf_project("Making Directories")
#' osf_mkdir(proj, "data")
#' osf_mkdir(proj, "data/raw_data")
#' osf_mkdir(proj, "super_raw_data", parent_id = "5be5e6a6fe3eca00188178f0")
#' }

osf_mkdir <- function(id, path, parent_id = NULL, verbose = FALSE) {
  id <- as_id(id)
  path_root <- fs::path_split(path)[[1]][1]
  items <- osf_ls(id, path_id = parent_id)

  if (path_root %in% items$name) {
    parent_id <- items$id[which(items$name == path_root)]
    if (verbose) {
      message(
        sprintf("%s (%s) already exists in node %s", path_root, parent_id ,id))
    }
  } else {
    new_dir <- .wb_create_folder(id, path_root, parent_id)
    parent_id <- gsub("/", "", new_dir$data$attributes$path, fixed = TRUE)
    if (verbose) {
      message(
        sprintf("Created %s (%s) in node %s", path_root, parent_id ,id))
    }
  }

  # recurse to the next-level if there is a subfolder
  path_next <- fs::path_rel(path, path_root)
  if (path_next != ".") {
    if (verbose) {
      message(
        sprintf("Continuing from %s to the next subfolder: %s",
                path_root, path_next))
    }
    out <- osf_mkdir(id, path = path_next, parent_id = parent_id)
  } else {
    out <- as_osf_tbl(.osf_file_retrieve(parent_id)[1], "osf_tbl_file")
  }
  out
}
