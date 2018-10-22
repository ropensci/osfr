# Return IDs for children of specified parent node or NULL if childless.
node_children <- function(id) {
  cli <- osf_cli()
  path <- osf_path(sprintf("nodes/%s/children/", id))
  res <- cli$get(path)
  res$raise_for_status()

  out <- jsonlite::fromJSON(res$parse("UTF-8"))
  if (length(out$data) == 0) return(NULL)
  out$data[, "id"]
}

