get.users <- function(id = NULL, user = NULL, password = NULL, nodes = FALSE){
  if (is.null(id)){
    raw <- httr::GET(construct.link("users"))

    result <- rjson::fromJSON(content(raw, 'text'))
  } else if (id == "me"){
    if(is.null(user)){
      warning("Please input username")}
    if(is.null(password)){
      warning("Please input password")}
    if(nodes == TRUE){
      raw <- httr::GET(construct.link("users/me/nodes"),
                 httr::authenticate(user, password))
    } else {raw <- httr::GET(construct.link("users/me"),
               httr::authenticate(user, password))
    }

    result <- rjson::fromJSON(content(raw, 'text'))
  } else {
    if(nodes == TRUE){
      raw <- httr::GET(construct.link(paste0("users/?filter[id]=", id, "/nodes")))
    } else{
      raw <- httr::GET(construct.link(paste0("users/?filter[id]=", id)))
      }

    result <- rjson::fromJSON(content(raw, 'text'))
  }

  return(result)
}

put.users <- function(id = NULL, user = NULL, password = NULL){}

patch.users <- function(id = 'me',
                        user = NULL,
                        password = NULL,
                        full_name = NULL,
                        given_name = NULL,
                        middle_names = NULL,
                        family_name = NULL,
                        suffix = NULL){

}
