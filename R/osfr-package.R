#' osfr: A package for interacting with OSF from the Center for Open Science.
#'
#' The osfr package helps you document your research as you go by creating
#' projects, uploading and downloading files, and much more.
#'
#' Suggestions for improvements or bug reports are always welcome at
#' \url{https://github.com/CenterForOpenScience/osfr}
#'
#' @docType package
#' @name osfr
#'
#' @importFrom crul HttpClient url_parse
#' @importFrom stringi stri_split_fixed
#' @importFrom purrr map map_chr imap_chr pluck set_names transpose %||%
#' @importFrom tibble tibble
#' @importFrom rlang abort warn is_empty is_scalar_character
#' @importFrom utils head tail modifyList browseURL
NULL
