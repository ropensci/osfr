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
#' @importFrom purrr discard
#' @noRd
unzip_files <- function(zipfiles, overwrite = FALSE) {
  stopifnot(all(fs::path_ext(zipfiles) == "zip"))
  stopifnot(all(file.exists(zipfiles)))

  zipped_files <- map(zipfiles, ~ unzip(.x, list = TRUE)$Name)

  # remove existing files to prevent overwriting
  if (isFALSE(overwrite)) {
    zipped_files <- map(zipped_files, purrr::discard, .p = file.exists)
  }

  unzipped <- Map(unzip, zipfile = zipfiles, files = zipped_files)
  unlink(zipfiles)
  invisible(unzipped)
}
