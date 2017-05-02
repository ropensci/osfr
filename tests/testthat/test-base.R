context("Base functions")

test_that("base link is proper", {
  lnk <- construct_link()
  expect_equal(substr(lnk, nchar(lnk), nchar(lnk)), "/")
  expect_equal(substr(lnk, 1, 8), "https://")
})

context("Setup API meta-interaction")

test_that("link construction is functional", {
  expect_that(construct_link(), matches("v2/$"))
  expect_that(construct_link("applications"), matches("applications$"))
})

test_that("login sets global environment", {
  expect_equal(login(pat = "12345"), "12345")
  expect_true(logout())
})
