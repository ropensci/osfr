context("Test the search functions")

test_that("searching osf", {
  # Make sure that multiword searches operate
  expect_that(class(search.nodes(title = 'ecological validity')), matches("data.frame"))
  expect_error(search.nodes(title = '*@#&#ASD'))
})

test_that("searching osf users", {
  # Make sure that multiword searches operate
  expect_that(class(search.users(family_name = 'nosek')), matches("data.frame"))
  expect_that(class(search.users(family_name = 'spies')), matches("data.frame"))
})

test_that("meta search = separate search", {
  expect_that(search.osf(type = 'nodes', title = 'ecological validity'),
              equals(search.nodes(title = 'ecological validity')))
  expect_that(search.osf(type = 'users', family_name = 'nosek'),
              equals(search.users(family_name = 'nosek')))
})
