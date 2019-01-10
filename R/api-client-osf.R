# Return a version specific user agent
user_agent <- function(agent = "osfr") {
  version <- system.file("DESCRIPTION", package = "osfr2", mustWork = FALSE)
  if (file.exists(version)) {
    version <- base::read.dcf(version, "Version")
    sprintf("%s v%s", agent, version)
  } else {
    agent
  }
}


# Appends API version to a specificed path
.osf_api_path <- function(path) {
  sprintf("v%s/%s", floor(.osf_api_version), path)
}

# Construct the OSF API Client
.osf_cli <- function(pat = getOption("osfr.pat")) {
  server <- Sys.getenv("OSF_SERVER")
  url <- if (nzchar(server)) {
    sprintf("https://api.%s.osf.io", server)
  } else {
    "https://api.osf.io"
  }

  headers <- list(
    `User-Agent` = user_agent(),
    `Accept-Header` = sprintf("application/vnd.api+json;version=%s", .osf_api_version)
  )

  if (!is.null(pat)) {
    headers$Authorization <- sprintf("Bearer %s", pat)
  }

  crul::HttpClient$new(
    url = url,
    opts = list(
      encode = "json"
    ),
    headers = headers
  )
}



# OSF API request functions -----------------------------------------------

.osf_request <- function(method, path, query = list(), body = NULL, verbose = FALSE, ...) {
  method <- match.arg(method, c("get", "put", "patch", "post", "delete"))
  cli <- .osf_cli()
  method <- cli[[method]]
  method(path, query, body = body, ...)
}

# TODO: .osf_request and .osf_paginated_request returns should be consistent
.osf_paginated_request <- function(method, path, query = list(), n_max = 100, verbose = FALSE) {
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
      if (i == 1) message(sprintf("Retrieving %i of %i available items:", n_max, total))
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
