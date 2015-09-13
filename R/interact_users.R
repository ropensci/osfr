#' Retrieve all users and their information from the OSF
#'
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

users.all <- function(){
  raw <- GET(construct.link("users"))

  result <- fromJSON(content(raw, 'text'))$data

  return(result)
}
