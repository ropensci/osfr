context("Identifiers")

test_that("as_id() detects one or more valid OSF GUIDs", {
  expect_s3_class(as_id("aaaaa"), "osf_id")
  expect_s3_class(as_id(c("aaaaa", "bbbbb")), "osf_id")
})

test_that("as_id() detects one or more valid Waterbutler IDs", {
  ids <- "565602398c5e4a3877d721a7"
  expect_s3_class(as_id(ids), "osf_id")
  ids <- c(ids, "553e671b8c5e4a219919e212")
  expect_s3_class(as_id(c("aaaaa", "bbbbb")), "osf_id")
})

test_that("GUIDs and Waterbutler IDs are detected in OSF URLs", {
  urls <- c(
    "https://osf.io/download/acpye/",
    "https://api.osf.io/v2/files/545602398c5e4a3877d721a8/",
    "https://files.osf.io/v1/resources/g81xl/providers/osfstorage/465602398c5e4a3877d721a8"
  )
  expect_s3_class(as_id(urls), "osf_id")
})

test_that("special identifier 'me' is recognized", {
  skip_if_no_pat()
  expect_match(id_type("me"), "users")
})
