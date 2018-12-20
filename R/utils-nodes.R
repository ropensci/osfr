#' Function to crawl through OSF project
#'
#' @param id OSF parent ID (osf.io/XXXXX) to crawl
#' @param maxdepth Integer, amount of levels deep to crawl
#'
#' @return Character vector of OSF IDs for children of the parent, with the
#'   parent ID listed last. If there are no child nodes to recurse, the parent
#'   ID is returned.

recurse_node <- function(id, maxdepth = 5) {
  node_tree <- recurse_tree(id, maxdepth)
  simplify_tree(
    set_names(list(node_tree), id)
  )
}

# Recursive function to traverse the nodes nested within the provided parent ID.
# Returns a nested list in which the name of each element corresponds to the
# relevant node ID. Each path terminates with a character vector of length 1
# containing the ID of the most deeply nested node.
recurse_tree <- function(id, maxdepth = 5) {
  if (maxdepth == 1) return(id)
  # TODO: use consistent argument names for number of items to return
  children <- osf_node_ls(id, n_max = maxdepth)
  if (length(children) == 0) return(id)
  purrr::map(
    .x = set_names(children$id, children$id),
    .f = recurse_tree,
    maxdepth = maxdepth - 1
  )
}

# Convert nested list to a character vector containing all node IDs, element
# names indicate each node's path in the original list.
simplify_tree <- function(x) {
  stopifnot(is.list(x))

  paths <- stringi::stri_split_fixed(names(unlist(x)), pattern = ".")

  node_list <- purrr::map(paths, function(x) {
    set_names(x, purrr::imap_chr(x, ~ paste0(x[seq_len(.y)], collapse = "/")))
  })

  nodes <- unlist(node_list)
  nodes[!duplicated(unname(nodes))]
}
