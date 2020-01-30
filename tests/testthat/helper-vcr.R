library(vcr)

# set to 'once' when recording new requests
record <- "once"

# redefine same global settings until the following issue is resolved
# https://github.com/ropensci/vcr/issues/136
vcr_config <- function(dir, record = "new_episodes") {
  invisible(
  vcr::vcr_configure(
    dir = file.path(rprojroot::find_testthat_root_file(), dir),
    record = record,
    match_requests_on = c("method", "uri"),
    filter_sensitive_data = list("<totallyrealpat>" = Sys.getenv("OSF_PAT")),
    log = nzchar(Sys.getenv("VCR_LOG")),
    log_opts = list(
      file = Sys.getenv("VCR_LOG"),
      log_prefix = "Cassette",
      date = TRUE
    )
  )
)
}
