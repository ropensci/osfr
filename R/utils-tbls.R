#' Check the data frame contains the required columns
#' @return logical
#' @noRd
has_osf_tbl_colnames <- function(x) {
  required <- c("name", "id", "meta")
  missing <- setdiff(required, colnames(x))
  is_empty(missing)
}

#' Check that required columns are the correct type
#' @return logical
#' @noRd
has_osf_tbl_coltypes <- function(x) {
  expected <- c(name = "character", id = "character", meta = "list")
  found <- vapply(x[names(expected)], FUN = typeof, FUN.VALUE = character(1L))
  incorrect <- found[expected != found]
  is_empty(incorrect)
}

#' Check that all rows are of the same OSF entity type
#' @return logical
#' @noRd
has_uniform_entity_type <- function(x) {
  types <- vapply(x$meta, determine_entity_type, FUN.VALUE = character(1))
  length(unique(types)) == 1
}

#' Determine OSF entity list type
#'
#' @param x OSF entity list containing links, relationships, and attributes
#'   slots
#' @return scalar character vector: "user", "node", or "file"
#' @noRd

determine_entity_type <- function(x) {
  stopifnot(is.list(x) && rlang::is_named(x))
  required_slots <- c("attributes", "links", "attributes")
  stopifnot(all(required_slots %in% names(x)))

  test_slots <- c(user = "family_name", node = "category", file = "kind")
  matched <- test_slots %in% names(x$attributes)
  if (sum(matched) == 1) {
    names(test_slots)[matched]
  } else {
    abort("Could not determine `x`'s entity type.")
  }
}

#' Validate OSF tibble
#'
#' Determine whether a data frame meets the requirements to be a valid
#' [`osf_tbl`] based on the names and types of included variables. If the data
#' frame is not empty, we also check the entity *type* represented by each row
#' to ensure the collection of entities is homogeneous.
#'
#' @param x an [`osf_tbl`]
#' @return Logical
#' @noRd
is_valid_osf_tbl <- function(x) {
  valid <- is.data.frame(x) &&
    has_osf_tbl_colnames(x) &&
    has_osf_tbl_coltypes(x)
  if (nrow(x) > 1) valid <- valid && has_uniform_entity_type(x)
  return(valid)
}
