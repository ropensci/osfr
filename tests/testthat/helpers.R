# for tests that must be executed on test.osf.io
on_test_server <- function() {
  Sys.getenv("OSF_SERVER") == "test"
}

skip_on_production_server <- function() {
  testthat::skip_if_not(
    on_test_server(),
    "OSF_SERVER not set to 'test'."
  )
}


# for tests that require authenticated access
has_pat <- function() {
  nzchar(Sys.getenv("OSF_PAT"))
}

skip_if_no_pat <- function() {
  testthat::skip_if_not(
    has_pat(),
    "No PAT detected."
  )
}
