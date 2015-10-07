get.applications <- function(id = NULL, user = NULL, password = NULL){
  if(is.null(user)){
    warning("Please input username")}
  if(is.null(password)){
    warning("Please input password")}

  if (is.null(id)){
    raw <- GET(construct.link("applications"), authenticate(user, password))

    result <- fromJSON(content(raw, 'text'))
  } else {
    raw <- GET(construct.link(paste0("applications/?filter[id]=", id)), authenticate(user, password))

    result <- fromJSON(content(raw, 'text'))
  }

  return(result)
}

post.applications <- function(){}

put.applications <- function(){}

patch.applications <- function(){}

delete.applications <- function(){}
