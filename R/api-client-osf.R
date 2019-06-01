# Return a version specific user agent
user_agent <- function(agent = "osfr") {
  version <- system.file("DESCRIPTION", package = "osfr", mustWork = FALSE)
  if (file.exists(version)) {
    version <- base::read.dcf(version, "Version")
    sprintf("%s v%s", agent, version)
  } else {
    agent
  }
}


# OSF API request functions -----------------------------------------------

.osf_request <-
  function(method,
           path,
           query = list(),
           body = NULL,
           verbose = FALSE,
           version = 2.8,
           ...) {

  method <- match.arg(method, c("get", "put", "patch", "post", "delete"))
  cli <- .build_client(api = "osf", encode = "json", version = version)

  cli$retry(
    method,
    prepend_version(path, version),
    query,
    body = body,
    times = 3,
    retry_only_on = "502",
    onwait = retry_message,
    ...
  )
}

# TODO: .osf_request and .osf_paginated_request returns should be consistent
.osf_paginated_request <-
  function(method,
           path,
           query = list(),
           n_max = 100,
           verbose = FALSE) {

  items <- list()
  i <- 1
  retrieved <- 0

  repeat {
    query <- modifyList(query, list(page = i))
    res <- .osf_request(method, path, query = query)
    out <- process_response(res)
    raise_error(out)

    total <- out$links$meta$total
    n_max <- ifelse(is.infinite(n_max), total, n_max)

    retrieved <- retrieved + length(out$data)
    items <- c(items, out$data)

    if (verbose && n_max > 10) {
      if (i == 1) {
        message(sprintf("Retrieving %i of %i available items:", n_max, total))
      }
      message(sprintf("..retrieved %i items", retrieved), appendLF = TRUE)
    }

    if (is.null(out$links$`next`) || retrieved >= n_max) {
      if (verbose && i > 1 && n_max > 10) message("..done")
      break
    }
    i <- i + 1
  }
  items
}
