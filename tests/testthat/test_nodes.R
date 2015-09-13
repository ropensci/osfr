library(httr)
suppressPackageStartupMessages(library(jsonlite, quietly=TRUE))

context("test nodes functions")

test_that("nodes.all returns object of class dataframe", {
	expect_equal(class(get.nodes.all()), 'data.frame')
	})