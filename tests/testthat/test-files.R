context("Files")

# setup -------------------------------------------------------------------
txt.file <- tempfile(pattern = "test-file-", fileext = ".txt")
writeLines("Lorem ipsum dolor sit amet, consectetur", con = txt.file)

p1 <- osf_project("File Tests")


# tests -------------------------------------------------------------------

test_that("non-existent file is detected", {
  expect_error(osf_upload(p1, "non-existent-file"), "File does not exist")
})

test_that("file is uploaded to project root", {
  f1 <- osf_upload(p1, txt.file)
  expect_s3_class(f1, "osf_file")
})
