construct.link <- function(id, type){
  base <- "https://staging2-api.osf.io/v2/"

  if (type == "applications")
    result <- sprintf("%s%s", base, link.applications(id))

  if (type == "nodes")
    result <- sprintf("%s%s", base, link.nodes(id))

  if (type == "users")
    result <- sprintf("%s%s", base, link.users(id))

  if (type == "files")
    result <- sprintf("%s%s", base, link.files(id))

  return(result)
}

link.applications <- function(client_id){
  result <- sprintf("applications/%s", client_id)

  return(result)
}

link.nodes <- function(node_id){
  result <- sprintf("nodes/%s", node_id)

  return(result)
}

link.users <- function(user_id){
  result <- sprintf("users/%s", user_id)

  return(result)
}

link.files <- function(file_id){
  result <- sprintf("files/%s", file_id)

  return(result)
}
