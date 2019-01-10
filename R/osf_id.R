#' Extract OSF identifiers
#'
#' Extract OSF GUIDs and Waterbutler IDs from various types of inputs. Valid
#' looking IDs are returned as `osf_id` objects.
#'
#' @section Identifier types:
#' There are 2 types of identifiers you'll encounter on OSF. The first is the
#' globally unique identifier, or GUID, that OSF assigns to every entity. A
#' valid OSF GUID consists of 5 alphanumeric characters. The second type of
#' identifier is specific to files stored on OSF. All file operations on OSF are
#' handled via Waterbutler. A valid Waterbutler ID consists of 24 alphanumeric
#' characters.
#'
#' @param x An `osf_tbl`, OSF URL, or a generic string containing a GUID or
#'   Waterbutler ID.
#'
#' @return A character vector with class `osf_id`.
#' @examples
#' \dontrun{
#' # extract a GUID from an OSF URL
#' proj_id <- as_id("https://osf.io/7zqxp/")
#'
#' # extract waterbutler IDs from an `osf_tbl_file`` with multiple files
#' osf_retrieve_node(proj_id) %>%
#'   osf_ls_files() %>%
#'   as_id()
#' }
#' @export
as_id <- function(x) UseMethod("as_id")

#' @export
as_id.character <- function(x) {

  # extract GUID from URLs
  ids <- purrr::map_chr(
    tolower(x),
    ~ ifelse(is_osf_url(.x), extract_osf_id(.x), .x)
  )

  # verify length is consistent with OSF GUID or waterbutler identifier
  invalid_ids <- !nchar(ids) %in% c(5L, 24L)

  if (any(invalid_ids)) {
    abort(paste(
      c(
        "Detected invalid OSF identifiers. See `?as_id` for more information.",
        sprintf("* Result %i is invalid", which(utils::head(invalid_ids, 10)))
      ),
      collapse = "\n"
    ))
  }

  structure(ids, class = "osf_id")
}

#' @export
as_id.osf_tbl <- function(x) as_id(x$id)

#' @export
as_id.osf_id <- function(x) x


#' Determine OSF ID entity type
#' @param id OSF or waterbutler ID
#' @return files, nodes, or users
#' @noRd
id_type <- function(id) {

  # is it a waterbutler ID?
  if (nchar(id) == 24) return("files")
  if (id == "me") return("users")

  out <- .osf_node_retrieve(id)
  if (is.null(out$errors)) return(out$data$type)

  out <- .osf_file_retrieve(id)
  if (is.null(out$errors)) return(out$data$type)

  out <- .osf_user_retrieve(id)
  if (is.null(out$errors)) return(out$data$type)

  stop("No OSF entity found for ID ", id)
}
