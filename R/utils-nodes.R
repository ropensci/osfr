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


# Recursive function to traverse the nodes nested within the provided parent ID.
# Returns a nested list in which the name of each element corresponds to the
# relevant node ID. Each path terminates with a character vector of length 1
# containing the ID of the most deeply nested node.
recurse_tree <- function(id, maxdepth = 5) {
  if (maxdepth == 1) return(id)
  child_ids <- node_children(id)
  if (is.null(child_ids)) return(id)
  purrr::map(
    .x = setNames(child_ids, child_ids),
    .f = recurse_tree,
    maxdepth = maxdepth - 1
  )
}

# Convert nested list to a character vector containing all node IDs, element
# names indicate each node's path in the original list.
simplify_tree <- function(x) {
  stopifnot(is.list(x))

  paths <- stringi::stri_split_fixed(names(unlist(x)), pattern = ".")
  node_list <- map(paths, function(x) {
    setNames(x, imap_chr(x, ~ paste0(x[seq_len(.y)], collapse = "/")))
  })

  nodes <- unlist(node_list)
  nodes[!duplicated(unname(nodes))]
}
