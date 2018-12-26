context("Authentication")

# Record developer PAT for OSF test server and unset variable for tests
test_pat <- Sys.getenv("OSF_PAT")
Sys.unsetenv("OSF_PAT")

test_that("osf_auth() warns no PAT is found", {
  expect_warning(osf_auth(), "No PAT found")
  expect_null(getOption("osfr.pat"))
})

test_that("osf_auth() defines osfr.pat from token arg", {
  options(osfr.pat = NULL)

  expect_message(
    osf_auth(test_pat),
    "Registered PAT from the provided token"
  )
  expect_equal(test_pat, getOption("osfr.pat"))
})

# Restore variables
Sys.setenv(OSF_PAT = test_pat)

test_that("osf_auth() defines osfr.pat from OSF_PAT", {
  options(osfr.pat = NULL)

  expect_message(
    osf_auth(),
    "Registered PAT from the OSF_PAT environment variable"
  )
  expect_equal(test_pat, getOption("osfr.pat"))
})
