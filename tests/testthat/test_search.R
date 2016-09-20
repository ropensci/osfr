context("Test the search functions")

test_that("searching osf", {
  # Make sure that multiword searches operate
  expect_that(class(search.osf(title = 'ecological validity')), matches("data.frame"))
  expect_error(search.osf(title = '*@#&#ASD'))
})

test_that("searching osf users", {
  # Make sure that multiword searches operate
  expect_that(class(search.users(family_name = 'nosek')), matches("data.frame"))
  expect_that(class(search.users(family_name = 'spies')), matches("data.frame"))
})
