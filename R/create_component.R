#' Create a component within a project
#'
#' @param id OSF id (osf.io/XXXX) of parent project
#' @param title Title of the component
#' @param description Description of the component
#' @param category Category of component, for valid categories
#'   see \code{\link{process_category}}
#' @param public Boolean of whether the component is supposed to be public
#'
#' @return OSF id of created component
#' @export
#' @seealso \code{\link{create_project}}
create_component <- function(
  id = NULL,
  title = "",
  description = "",
  category = "",
  public = TRUE,
  ...) {

  if (Sys.getenv("OSF_PAT") == "")
    stop("Requires login, use login()")
  if (is.null(id))
    stop("Please input project id.")
  process_category(category)

  url.osf <- construct_link(sprintf("nodes/%s/children/", id), ...)

  # Create the JSON body
  body <- list(
    data = list(
      type = "nodes",
      attributes = list(
        title = title,
        category = category,
        description = description,
        public = public
      )
    )
  )

  call <- httr::POST(
    url = url.osf,
    body = body, encode = "json",
    httr::add_headers(Authorization = sprintf("Bearer %s", login())))

  if (call$status_code != 201)
    stop("Failed in creating new component.")

  res <- process_json(call)
  id <- res$data$id

  return(id)
}
