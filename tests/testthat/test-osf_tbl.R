context("osf_tbl construction")

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


context("osf_tbl validation")

user_tbl <- osf_retrieve_user("me")
user_tbl$foo <- "bar"

test_that("valid osf_tbls are passed through validation", {
  expect_s3_class(rebuild_osf_tbl(user_tbl), "osf_tbl_user")
})

test_that("osf_tbls missing required columns are detected", {
  expect_true(has_osf_tbl_colnames(user_tbl))
  expect_false(has_osf_tbl_colnames(user_tbl[-1]))
  expect_false(has_osf_tbl_colnames(user_tbl[-2]))
  expect_false(has_osf_tbl_colnames(user_tbl[-3]))
})

test_that("osf_tbls with incorrect column types are detected", {
  user_tbl$id <- as.factor(user_tbl$id)
  expect_false(has_osf_tbl_coltypes(user_tbl))
})
