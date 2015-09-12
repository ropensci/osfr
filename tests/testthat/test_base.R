context("Base link")

test_that("base link is proper", {
	expect_equal(substr(construct.link(),
		start = nchar(construct.link()),
		stop = nchar(construct.link())),
		'/')
	expect_equal(substr(construct.link(),
		start = 1,
		stop = 8),
		'https://')
	})