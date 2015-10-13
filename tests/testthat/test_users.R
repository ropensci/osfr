context("test users functions")

test_that("users returns object of class list", {
	expect_that(class(get.users()), 'list')
	expect_error(get.users('me'))
	})