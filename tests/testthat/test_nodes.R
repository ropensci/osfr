library(httr)
suppressPackageStartupMessages(library(jsonlite, quietly=TRUE))

context("test nodes functions")

test_that("get.nodes returns object of class dataframe", {
	expect_equal(class(get.nodes()), 'data.frame')
	})