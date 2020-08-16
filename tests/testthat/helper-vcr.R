library(vcr)

# make path relative to test directory
cassette_dir <- function(x) {
  file.path(rprojroot::find_testthat_root_file(), "cassettes", x)
}

vcr::vcr_configure(
  # set to 'once' when recording new requests
  record = "once",
  match_requests_on = c("method", "uri", "body"),
  filter_sensitive_data = list("<totallyrealpat>" = Sys.getenv("OSF_PAT")),
  log = nzchar(Sys.getenv("VCR_LOG")),
  log_opts = list(
    file = Sys.getenv("VCR_LOG"),
    log_prefix = "Cassette",
    date = TRUE
  )
)
