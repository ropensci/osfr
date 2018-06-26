context('account operations')

test_that("login works", {
  # Get an error if login does not have a PAT
  expect_error(login())

  # login() changes the environmental variable to the PAT
  Sys.setenv(OSF_PAT = "")
  login(pat = osf_pat)
  expect_equal(osf_pat, Sys.getenv("OSF_PAT"))
})

test_that("auth works", {
  # auth() returns the system environment variable OSF_PAT
  login(pat = osf_pat)
  expect_equal(auth(), osf_pat)
})

test_that("logout works", {
  # logout() sets the system environment variable OSF_PAT to ""
  login(pat = osf_pat)
  logout()
  expect_equal(Sys.getenv("OSF_PAT"), "")
})
