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
  title = "",
  description = "",
  private = TRUE) {

  config <- get_config(TRUE)

  url_osf <- construct_link("nodes/")

  # Create the JSON body
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

  if (call$status_code != 201)
    stop("Failed in creating new project.")

  res <- process_json(call)
  id <- res$data$id

  return(id)
}

# update_project <- function() {
# }

clone_project <- function(id, private = FALSE, maxdepth = 5) {
	get_config(private)

	tmp <- recurse_node(id, private, maxdepth, path_id = TRUE)

	plyr::daply(tmp, "path", .progress = 'text', function (x) {
		dir.create(x$path, recursive = TRUE)

		files <- get_file_info(x$id)

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
}

#' Delete a project from the OSF
#'
#' @param id OSF id (osf.io/xxxx)
#' @param recursive Boolean, if TRUE will go through folder nesting (see \code{maxdepth})
#' @param maxdepth Number of nesting levels to go through
#'
#' @return Boolean, delete succeeded?
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
#' @param id OSF id (osf.io/xxxx)
#'
#' @export
#' @importFrom utils browseURL

view_project <- function(id) {
  utils::browseURL(paste0("https://osf.io/", id))
}
