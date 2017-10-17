context("Test whether commenting works")

test_that("login, welcome, and logout work", {
  x <- create_project('tmp')
  expect_error(comment_osf())
  expect_error(comment_osf(id = x))
  expect_true(comment_osf(id = x, txt = "Testing the function comment_osf()"))
})
