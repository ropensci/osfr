construct.link <- function(request){
  base <- "https://staging2-api.osf.io/v2/"

  result <- sprintf("%s%s", base, request)

  return(result)
}
