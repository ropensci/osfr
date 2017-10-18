#' Create a project on the OSF
#'
#' @param title Project title
#' @param description Project description
#' @param private Boolean, whether project should be private
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
  title,
  description = '',
  private = TRUE) {

  config <- get_config(TRUE)
  url_osf <- construct_link("nodes/")

  body <- list(
    data = list(
      type = "nodes",
      attributes = list(
        title = title,
        category = "project",
        description = description,
        public = !private
      )
    )
  )

  call <- httr::POST(url = url_osf, body = body, encode = "json", config)

  if (call$status_code != 201) {
    stop("Failed in creating new project.")
  }

  res <- process_json(call)
  id <- res$data$id

  return(id)
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
#'

update_project <- function(id, private = FALSE) {
  config <- get_config(TRUE)
  url_osf <- construct_link(sprintf("nodes/%s/", id))

  body <- list(
    data = list(
      type = "nodes",
      id = id,
      attributes = list(
        public = !private
      )
    )
  )

  call <- httr::PATCH(url = url_osf, body = body, encode = "json", config)
  if (call$status_code != 200) {
    stop("Failed in updating project.")
  }

  return(TRUE)
}

#' Clone OSF project to desktop
#'
#' This function copies an *entire* project and its
#' components to the harddrive of the individual
#' (depth of clone depends on the maxdepth argument).
#' There is currently no way to estimate the size
#' before downloading so it might take a while. BUT
#' there's a progess bar :-) Currently limited to only
#' files stored on OSF (not via add-ons).
#'
#' @param id OSF id (osf.io/XXXX; just XXXX)
#' @param private clone the project as seen privately?
#' @param maxdepth how many levels of subcomponents to trawl
#'
#' @return Boolean, clone success
#' @export
#'

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

#' Delete a project from the OSF
#'
#' @param id OSF id (osf.io/xxxx)
#' @param recursive Boolean, if TRUE will go through folder nesting (see \code{maxdepth})
#' @param maxdepth Number of nesting levels to go through
#'
#' @return Boolean, delete success
#' @export

delete_project <- function(id, recursive = FALSE, maxdepth = 5) {
  if (recursive) {
    del_id <- recurse_node(id, private = TRUE, maxdepth)
    for (each in del_id) {
      delete_component(each)
      message(sprintf("Deleted subcomponent %s", each))
    }
  } else {
    delete_component(id)
  }

  return(TRUE)
}

#' View an OSF project on osf.io
#'
#' @param id OSF id (osf.io/XXXX; just XXXX)
#'
#' @export
#' @importFrom utils browseURL

view_project <- function(id) {
  utils::browseURL(paste0("https://osf.io/", id))
}
