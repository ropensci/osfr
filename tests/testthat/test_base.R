# context("Base link")
#
# test_that("base link is proper", {
# 	expect_equal(substr(construct_link(),
# 		start = nchar(construct_link()),
# 		stop = nchar(construct_link())),
# 		'/')
# 	expect_equal(substr(construct_link(),
# 		start = 1,
# 		stop = 8),
# 		'https://')
# 	})
