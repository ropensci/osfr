construct.link <- function(request = NULL){
  base <- "https://staging2-api.osf.io/v2/"

  result <- paste0(base, request)

  return(result)
}
