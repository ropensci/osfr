# dplyr compatibility is adapted from dtplyr and googledrive
# https://github.com/hadley/dtplyr/blob/master/R/compat-dplyr-0.6.0.R
# https://github.com/tidyverse/googledrive/blob/master/R/dplyr-compat.R

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(
    is_scalar_character(pkg) &&
    is_scalar_character(generic) &&
    is_scalar_character(class)
  )

  envir <- asNamespace(pkg)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))


  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
}

arrange.osf_tbl <- function(.data, ...) rebuild_osf_tbl(NextMethod())
filter.osf_tbl <- function(.data, ...)  rebuild_osf_tbl(NextMethod())
mutate.osf_tbl <- function(.data, ...)  rebuild_osf_tbl(NextMethod())
rename.osf_tbl <- function(.data, ...)  rebuild_osf_tbl(NextMethod())
select.osf_tbl <- function(.data, ...)  rebuild_osf_tbl(NextMethod())
slice.osf_tbl <- function(.data, ...)   rebuild_osf_tbl(NextMethod())
