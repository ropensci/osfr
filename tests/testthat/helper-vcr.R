library(vcr)

invisible(
  vcr::vcr_configure(
    dir = "tests/testthat/cassettes",
    match_requests_on = c("method", "uri", "body"),
    filter_sensitive_data = list("<totallyrealpat>" = Sys.getenv("OSF_PAT")),
    log = nzchar(Sys.getenv("VCR_LOG")),
    log_opts = list(
      file = Sys.getenv("VCR_LOG"),
      log_prefix = "Cassette",
      date = TRUE
    )
  )
)

