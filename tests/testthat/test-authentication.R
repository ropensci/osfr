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
    osf_auth("fake_token"),
    "Registered PAT from the provided token"
  )
  expect_equal("fake_token", getOption("osfr.pat"))
})


test_that("osf_auth() defines osfr.pat from OSF_PAT", {
  withr::local_envvar(OSF_PAT = "fake_token")
  withr::local_options(osfr.pat = NULL)

  expect_message(
    osf_auth(),
    "Registered PAT from the OSF_PAT environment variable"
  )
  expect_equal("fake_token", getOption("osfr.pat"))
})
