library(httr)
library(jsonlite)

context("test users functions")

test_that("users returns object of class dataframe", {
	expect_equal(class(get.users()), 'data.frame')
	})