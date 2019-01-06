#' osfr: R interface to OSF
#'
#' osfr provides a suite of functions for interacting with
#' [OSF](https://www.osf.io) that are primarily focused on project management
#' workflows.
#'
#' @importFrom crul HttpClient url_parse
#' @importFrom stringi stri_split_fixed
#' @importFrom purrr map map_chr imap_chr pluck set_names transpose %||%
#' @importFrom tibble tibble
#' @importFrom rlang abort warn is_empty is_scalar_character
#' @importFrom utils head tail modifyList browseURL
"_PACKAGE"
