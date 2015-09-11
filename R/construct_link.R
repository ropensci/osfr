construct.link <- function(request, type){
  base <- "https://staging2-api.osf.io/v2/"

  if (type == "applications")
    result <- sprintf("%s%s", base, link.applications(request))

  return(result)
}

link.applications <- function(client_id){
  result <- sprintf("applications/%s", client_id)

  return(result)
}

link.nodes <- function(node_id){

}

link.users <- function(user_id){

}

link.files <- function(file_id){

}
