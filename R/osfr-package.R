#' osfr: R interface to OSF
#'
#' osfr provides a suite of functions for interacting with the Open Science
#' Framework (OSF; <https://osf.io/>).
#'
#' @section What is OSF?:
#'
#' OSF is a free and open source project management repository designed to
#' support researchers across their entire project lifecycle. The service
#' includes free cloud storage and file version history, providing a
#' centralized location for all your research materials that can be kept
#' private, shared with select collaborators, or made publicly available with
#' citable DOIs.
#'
#' Most work on OSF is organized around ***projects***. Projects can contain
#' *files*, groups of files in *directories*, and/or files in sub-projects
#' called ***components***. Note there is no storage limit on the size of
#' projects but individual files must be < 5Gb.
#'
#' @section Resources:
#'
#' * To learn more about OSF check out the helpful series of guides published by
#' the Center for Open Science: <https://help.osf.io>
#' * See the vignette for an overview of osfr's features:
#' \code{vignette("getting_started", package = "osfr")}
#'
#'
#' @importFrom crul HttpClient url_parse
#' @importFrom httr progress
#' @importFrom stringi stri_split_fixed
#' @importFrom purrr map map_chr imap_chr chuck set_names transpose %||%
#' @importFrom tibble tibble
#' @importFrom rlang abort warn is_empty is_scalar_character
#' @importFrom utils head tail modifyList browseURL unzip
"_PACKAGE"
