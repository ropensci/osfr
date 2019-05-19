#' @examples
#' p1 <- osf_create_project("Upload Test")
#' out <- osf_multi_upload(p1, c("appveyor.yml", "tests", "README.md"))
#' osf_rm(p1, check = FALSE)

osf_multi_upload <-
  function(x,
           paths,
           overwrite = TRUE,
           verbose = TRUE) {

  paths <- check_files(paths)

  # memoise directory retrieval to avoid subsequent API calls for every
  # file or subdirectory contained therein
  get_path <- memoise::memoise(
    function(x, path) {
      message(sprintf("Getting path `%s` from `%s`", path, as_id(x)))
      recurse_path(x, path, "create", verbose)
    }
  )

  # split into top-level files and folders
  paths_by <- list()
  paths_by[c("files", "folders")] <- split(paths, fs::is_dir(paths))

  # upload files in pwd
  if (!is.null(paths_by$files)) {
    out_files <- map(
      paths_by$files,
      ~ osf_upload(x, .x, overwrite = overwrite, verbose = verbose)
    )
  } else {
    out_files <- NULL
  }

  if (!is.null(paths_by$folders)) {
    out_folders <- fs::dir_walk(paths_by$folders, recursive = TRUE, fun = function(file) {
        message(sprintf("Working on %s", file))
        # if path is a directory, create or retrieve the corresponding osf folder
        # and return it. if path is a file, retrieve its osf parent and upload there
        if (fs::is_dir(file)) {
          get_path(x, file)
        } else {
          parent_path <- dirname(file)
          if (parent_path == ".") {
            parent <- x
          } else {
            parent <- get_path(x, parent_path)
          }
          osf_upload(parent, file, overwrite = overwrite, verbose = verbose)
        }
      }
    )
    # dir_walk() returns directory names, so we need to re-retrieve them
    out_folders <- map(out_folders, get_path, x = x)
  } else {
    out_folders <- NULL
  }

  memoise::forget(get_path)
  do.call("rbind", c(out_files, out_folders))
}
