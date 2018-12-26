context("osf_tbl class")

test_that("NULL input returns empty osf_tbl", {
  out <- osf_tbl()
  expect_s3_class(out, "osf_tbl")
  expect_equal(nrow(out), 0)
})

test_that("empty list returns empty osf_tbl", {
  out <- osf_tbl(list())
  expect_s3_class(out, "osf_tbl")
  expect_equal(nrow(out), 0)
})
