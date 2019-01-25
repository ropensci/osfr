context("Authentication")


# setup -------------------------------------------------------------------

# Record developer PAT for OSF test server and unset variable for tests
setup({
  if (has_pat()) {
    test_pat <<- Sys.getenv("OSF_PAT")
    Sys.unsetenv("OSF_PAT")
  }
})

# Restore variables
teardown({
  Sys.unsetenv("OSF_PAT")
  options(osfr.pat = NULL)

  if (exists("test_pat")) {
    Sys.setenv(OSF_PAT = test_pat)
    suppressMessages(osf_auth())
  }
})


# tests -------------------------------------------------------------------
test_that("osf_auth() warns no PAT is found", {
  expect_warning(osf_auth(), "No PAT found")
  expect_null(getOption("osfr.pat"))
})

test_that("osf_auth() defines osfr.pat from token arg", {
  options(osfr.pat = NULL)

  expect_message(
    osf_auth("fake_token"),
    "Registered PAT from the provided token"
  )
  expect_equal("fake_token", getOption("osfr.pat"))
})


test_that("osf_auth() defines osfr.pat from OSF_PAT", {
  Sys.setenv(OSF_PAT = "fake_token")
  options(osfr.pat = NULL)

  expect_message(
    osf_auth(),
    "Registered PAT from the OSF_PAT environment variable"
  )
  expect_equal("fake_token", getOption("osfr.pat"))
})
