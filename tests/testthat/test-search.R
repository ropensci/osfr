context("Test the search functions")

test_that("searching osf", {
  # Make sure that multiword searches operate
  expect_s3_class(search_nodes(title = "ecological validity"), "data.frame")
  expect_error(search_nodes(title = "*@#&#ASD"))
})

test_that("searching osf users", {
  # Make sure that multiword searches operate
  expect_s3_class(search_users(family_name = "nosek"), "data.frame")
  expect_s3_class(search_users(family_name = "spies"), "data.frame")
})

test_that("meta search = separate search", {
  expect_identical(
    search_osf(type = "nodes", title = "ecological validity"),
    search_nodes(title = "ecological validity"))
  expect_identical(
    search_osf(type = "users", family_name = "nosek"),
    search_users(family_name = "nosek"))
})
