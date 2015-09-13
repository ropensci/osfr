context("Authentication procedure")

test_that("authentication is functional", {
	expect_true(osf.authenticate())
	})