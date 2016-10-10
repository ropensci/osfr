context("Setup API meta-interaction")

test_that("link construction is functional", {
	expect_that(construct_link(), matches("v2/$"))
  expect_that(construct_link(test = TRUE), matches("v2/$"))
	expect_that(construct_link('applications'), matches("applications$"))
	expect_that(construct_link('applications', test = TRUE), matches("applications$"))
	})

test_that("welcome message operates", {
	expect_warning(welcome(), 'Currently not logged in')
  # Set the test PAT before testing
  # Make sure to have object OSF_PAT_TEST in environment
  # I usually keep it in ~/.Rprofile
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  expect_that(class(suppressMessages(welcome(test = TRUE))), matches('list'))
  logout()
})

test_that("login sets global environment", {
  expect_equal(login(pat = '12345'), '12345')
  expect_true(logout())
})

test_that("check_type functions", {
  expect_that(check_type(id = 'djxwq', test = TRUE), matches('nodes'))
  expect_that(check_type(id = 'kjh47', test = TRUE), matches('files'))
  # Also checkable for private nodes/files
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  expect_that(check_type(id = '2pf6w', private = TRUE, test = TRUE), matches('files'))
  logout()
})
