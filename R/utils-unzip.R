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

  # unzip to a directory named for the zipfile
  exdirs <- map(zipfiles, fs::path_ext_remove)
  # unzip to pwd if the exdir is embedded in the zip file
  redundant <- purrr::map2_lgl(zipped_files, exdirs, ~ fs::path_has_parent(.x, .y))
  exdirs <- ifelse(redundant, ".", exdirs)

  unzipped <- Map(unzip, zipfile = zipfiles, files = zipped_files, exdir = exdirs)

  unlink(zipfiles)
  invisible(unzipped)
}
