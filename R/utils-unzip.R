#' Safely unzip multiple zipfiles
#'
#' This allows for unzipping multiple zipfiles and works around a bug present in
#' R <= 3.5.1 where `overwrite = FALSE` is ignored by `unzip()`. The unzip
#' location is always the same as the zipfile's.
#'
#' @param zipfiles a character vector of paths to one or more zipfiles
#' @param overwrite should unzipped files overwrite existing files?
#'
#' @return Invisibly returns a named list for each `zipfile` containing the
#'   unzipped file paths
#' @noRd
unzip_files <- function(zipfiles, overwrite = FALSE) {
  stopifnot(all(fs::path_ext(zipfiles) == "zip"))
  stopifnot(all(file.exists(zipfiles)))

  zipped_files <- map(zipfiles, ~ unzip(.x, list = TRUE)$Name)
  extract_dirs <- map(zipfiles, fs::path_ext_remove)

  # unzip to pwd if the exdir is embedded in the zip file
  redundant <- purrr::map2_lgl(
    .x = zipped_files,
    .y = extract_dirs,
    ~ fs::path_has_parent(.x, .y)
  )
  extract_dirs <- ifelse(redundant, ".", extract_dirs)

  # remove existing files to prevent overwriting
  if (isFALSE(overwrite)) {
    zipped_files <- Map(
      function(file, dir) {
        path <- file.path(dir, file)
        file[!file.exists(path)]
      },
      file = zipped_files,
      dir = extract_dirs
    )
  }

  unzipped <- Map(unzip, zipfile = zipfiles, files = zipped_files, exdir = extract_dirs)

  unlink(zipfiles)
  invisible(unzipped)
}
