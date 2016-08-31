context("Setup API meta-interaction")

test_that("link construction is functional", {
	expect_that(construct.link(), matches("v2/$"))
  expect_that(construct.link(test = TRUE), matches("v2/$"))
	expect_that(construct.link('applications'), matches("applications$"))
	expect_that(construct.link('applications', test = TRUE), matches("applications$"))
	})

test_that("welcome message operates", {
	expect_warning(welcome(), 'Currently not logged in')
  # Set the test PAT before testing
  # Make sure to have object OSF_PAT_TEST in environment
  # I usually keep it in ~/.Rprofile
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  expect_that(class(suppressMessages(welcome(test = TRUE))), matches('list'))
  logout
})

test_that("login sets global environment", {
  expect_equal(login(pat = '12345'), '12345')
  expect_true(logout())
})
