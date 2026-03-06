# tests -------------------------------------------------------------------
test_that("osf_auth() warns no PAT is found", {
  withr::local_envvar(OSF_PAT = NA)
  withr::local_options(osfr.pat = NULL)

  expect_warning(osf_auth(), "No PAT found")
  expect_null(getOption("osfr.pat"))
})

test_that("osf_auth() defines osfr.pat from token arg", {
  withr::local_envvar(OSF_PAT = NA)
  withr::local_options(osfr.pat = NULL)

  expect_message(
    suppressWarnings(osf_auth("fake_token")),
    "Registered PAT from the provided token"
  )
  expect_equal("fake_token", getOption("osfr.pat"))
})


test_that("osf_auth() defines osfr.pat from OSF_PAT", {
  withr::local_envvar(OSF_PAT = "fake_token")
  withr::local_options(osfr.pat = NULL)

  expect_message(
    suppressWarnings(osf_auth()),
    "Registered PAT from the OSF_PAT environment variable"
  )
  expect_equal("fake_token", getOption("osfr.pat"))
})


# Token format validation -------------------------------------------------

test_that("osf_auth() warns on unexpected token length", {
  withr::local_envvar(OSF_PAT = NA)
  withr::local_options(osfr.pat = NULL)

  expect_warning(
    suppressMessages(osf_auth("short_token")),
    "unexpected length"
  )
})

test_that("osf_auth() does not warn for 70-character token", {
  withr::local_envvar(OSF_PAT = NA)
  withr::local_options(osfr.pat = NULL)

  expect_no_warning(
    suppressMessages(osf_auth(strrep("a", 70)))
  )
})
