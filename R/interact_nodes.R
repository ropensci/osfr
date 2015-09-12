nodes.all <- function(){
  raw <- GET(construct.link("nodes"))

  result <- fromJSON(content(raw, 'text'))$data

  return(result)
}
