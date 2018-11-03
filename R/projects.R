#' Create a project on OSF
#'
#' @param title Project title
#' @param description Project description
#' @param private Boolean, whether project should be private (defaults to
#' \code{TRUE})
#'
#' @return Returns the created project's id
#' @export
#' @seealso \code{\link{create_component}}
#'
#' @examples
#' \dontrun{
#' create_project(title = "New Private OSF Project")
#' create_project(title = "New Public OSF Project", private = FALSE)}

create_project <- function(
  title,
  description = '',
  private = TRUE) {

  if (missing(title)) stop("Specify a project title")
  path <- osf_path("nodes/")
  out <- create_node(path, title, description, private)

  structure(
    .Data = out$data$id,
    links = out$data$links,
    attributes = out$data$attributes,
    relationships = out$data$relationships,
    class = paste0("osf_", out$data$attributes$category)
  )
}

#' Update an OSF project
#'
#' This updates some of the metadata for an OSF project.
#' Currently limited to switching a project to public.
#' Does not yet include making public all subcomponents
#' or vectorized to make multiple projects public at the
#' same time.
#'
#' @param id OSF id (osf.io/XXXX; just XXXX)
#' @param private Set project to private/public (default changes to public)
#'
#' @return Boolean of update success
#' @export
#' @examples
#' \dontrun{
#' update_project(id = "12345")}

update_project <- function(id, title = NULL, description = NULL, private = NULL) {
  out <- update_node(id, title, description, private)
  out$data$id
}

#' Clone OSF project to desktop
#'
#' This function copies an \emph{entire} project and its
#' components to the harddrive of the individual
#' (depth of clone depends on the maxdepth argument).
#' There is currently no way to estimate the size
#' before downloading so it might take a while. BUT
#' there's a progess bar :-) Currently limited to only
#' files stored on OSF (not via add-ons).
#'
#' @param id OSF id (osf.io/XXXXX; just XXXXX)
#' @param private clone the project as seen privately?
#' @param maxdepth how many levels of subcomponents to trawl
#'
#' @return Boolean, clone success
#' @export
#' @examples
#' \dontrun{
#' clone_project(id = "12345")}

clone_project <- function(id, private = FALSE, maxdepth = 5) {
  get_config(private)
  tmp <- recurse_node(id, private, maxdepth, path_id = TRUE)

  plyr::daply(tmp, "path", .progress = 'text', function (x) {
    dir.create(x$path, recursive = TRUE)
    files <- get_files_info(x$id)

    apply(files, 1, function (y) {
      href <- y[which(names(y) == "href")]
      path <- y[which(names(y) == "materialized")]
      type <- y[which(names(y) == "kind")]

      if (type == 'folder') {
        dir.create(paste0(x$path, path), recursive = TRUE)
      } else {
        invisible(httr::GET(href,
                            httr::write_disk(paste0(x$path, path), overwrite = TRUE)))
      }
    })
  })

  return(TRUE)
}


#' View an OSF project on osf.io
#'
#' This function opens the project in a web browser. If the project is private,
#' you will be asked to log into OSF from the browser.
#'
#' @param id OSF id (osf.io/XXXXX; just XXXXX)
#'
#' @export
#' @importFrom utils browseURL
#' @examples
#' \dontrun{
#' view_project(id = "m5pds")}

view_project <- function(id) {
  utils::browseURL(paste0("https://osf.io/", id))
}
