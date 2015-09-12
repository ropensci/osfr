library(httr)
library(jsonlite)

context("test nodes functions")

test_that("nodes.all returns object of class dataframe", {
	expect_equal(class(nodes.all()), 'data.frame')
	})