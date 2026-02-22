library(vcr)

# Cassettes are recorded against the test server so ensure OSF_SERVER is set
if (!nzchar(Sys.getenv("OSF_SERVER"))) {
  withr::local_envvar(OSF_SERVER = "test", .local_envir = teardown_env())
}

cassette_dir <- function(x) {
  vcr::vcr_test_path("cassettes", x)
}

vcr::vcr_configure(
  record = "once"
)

if (nzchar(Sys.getenv("VCR_LOG"))) {
  vcr::vcr_configure_log(file = Sys.getenv("VCR_LOG"))
}
