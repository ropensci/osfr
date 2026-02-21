library(vcr)

cassette_dir <- function(x) {
  vcr::vcr_test_path("cassettes", x)
}

vcr::vcr_configure(
  record = "once"
)

if (nzchar(Sys.getenv("VCR_LOG"))) {
  vcr::vcr_configure_log(file = Sys.getenv("VCR_LOG"))
}
