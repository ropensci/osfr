#' #' Retrieve additional attributes for an OSF object
#' #'
#' #' @section Node Attributes
#' #' - category
#' #' - date_modified
#' #' - tags
#' #' - fork
#' #' - description
#' #' - collection
#' #' - license
#' #' - public
#' #' - date_created
#' #' - wiki_enabled
#' #' @noRd
#' osf_get <- function(x, attributes) UseMethod("osf_get")
#'
#' osf_get.default <- function(x, attributes) {
#'   stop(
#'     sprintf("Don't know how to retrieve metadata for object of class %s", x)
#'   )
#' }
#'
#' osf_get.osf_tbl_node <- function(x, attributes) {
#'   browser()
#'   attrs <-
#'     c(
#'       "category",
#'       "fork",
#'       "current_user_is_contributor",
#'       "preprint",
#'       "description",
#'       "current_user_permissions",
#'       "custom_citation",
#'       "access_requests_enabled",
#'       "date_modified",
#'       "collection",
#'       "public",
#'       "subjects",
#'       "registration",
#'       "date_created",
#'       "current_user_can_comment",
#'       "node_license",
#'       "wiki_enabled",
#'       "tags"
#'     )
#'
#'   valid <- intersect(attributes, attrs)
#'   invalid <- setdiff(attributes, attrs)
#'
#'   metadata <- lapply(x$meta, function(x) x$attributes[valid])
#'
#'
#'   purrr::map_df(metadata, as_data_frame)
#'
#' }
#' #
#' # osf_get.osf_tbl_file <-
#' #
#' # osf_get.osf_tbl_user
#'
#'
