context("Test whether logging in, out, and cleaning up works")

base::Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))

test_that("login, welcome, and logout work", {
  skip_on_travis()
  expect_is(login(), "character")
  expect_warning(welcome())
  expect_null(welcome(test = TRUE))
  expect_true(logout())
})
