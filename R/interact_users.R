users.all <- function(){
  raw <- GET(construct.link("users"))

  result <- fromJSON(content(raw, 'text'))$data

  return(result)
}
