#' Retrieve all users and their information from the OSF
#'
#' @param id The user_id to search for. If NULL (default), returns all users.
#' @return Object dataframe including, for each user:
#' \enumerate{
#' \item id
#' \item fullname
#' \item given_name
#' \item middle_names
#' \item family_name
#' \item suffix
#' \item date_registered
#' \item profile_image_url
#' \item gitHub
#' \item scholar
#' \item personal_website
#' \item twitter
#' \item linkedIn
#' \item impactStory
#' \item orcid
#' \item researcherId
#' \item links
#' \item nodes
#' }

get.users <- function(id = NULL){
  if (is.null(id)){
    raw <- GET(construct.link("users"))

    result <- fromJSON(content(raw, 'text'))$data

    return(result)
  }
}
