#' Create a project on the OSF
#'
#' @param title Project title
#' @param description Project description
#' @param public Boolean, whether project should be public (FALSE for private)
#'
#' @return Returns the created project's id
#' @export
#' @seealso \code{\link{create_component}}
#'
#' @examples
#' \dontrun{
#' create_project(title = "Testing the OSF project creation")
#' }
create_project <- function(
  title = "",
  description = "",
  public = TRUE,
  ...) {

  if (Sys.getenv("OSF_PAT") == "")
    stop("Requires login, use login()")

  url.osf <- construct_link("nodes/", ...)

  # Create the JSON body
  body <- list(
    data = list(
      type = "nodes",
      attributes = list(
        title = title,
        category = "project",
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
    stop("Failed in creating new project.")

  res <- process_json(call)
  id <- res$data$id

  return(id)
}
