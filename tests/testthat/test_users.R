context("test users functions")

test_that("users are retrieved", {
  expect_that(class(get.users()), matches('list'))
})
