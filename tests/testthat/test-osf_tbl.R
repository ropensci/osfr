# setup -------------------------------------------------------------------
vcr::vcr_configure(dir = cassette_dir("osf-tbl"))

vcr::use_cassette("retrieve-user", {
  user_tbl <- osf_retrieve_user("me")
})

# tests -------------------------------------------------------------------
test_that("NULL input returns empty osf_tbl", {
  out <- osf_tbl()
  expect_s3_class(out, "osf_tbl")
  expect_equal(nrow(out), 0)
  expect_true(is_valid_osf_tbl(out))
})

test_that("empty list returns empty osf_tbl", {
  out <- osf_tbl(list())
  expect_s3_class(out, "osf_tbl")
  expect_equal(nrow(out), 0)
  expect_true(is_valid_osf_tbl(out))
})

# osf_tbl validation ------------------------------------------------------

test_that("valid osf_tbls are passed through validation", {
  user_tbl_with_foo <- user_tbl
  user_tbl_with_foo$foo <- "bar"

  expect_s3_class(rebuild_osf_tbl(user_tbl_with_foo), "osf_tbl_user")
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

test_that("can't combine osf_tbls with different subclasses", {
  vcr::use_cassette("retrieve-node", {
    proj_tbl <- osf_retrieve_node("brfza")
  })
  out <- rbind(user_tbl, proj_tbl)
  expect_identical(class(out), c("tbl_df", "tbl", "data.frame"))
})
