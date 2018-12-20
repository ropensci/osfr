context("Uploading")

# setup -------------------------------------------------------------------
txt.file <- tempfile(pattern = "test-file-", fileext = ".txt")
writeLines("Lorem ipsum dolor sit amet, consectetur", con = txt.file)

p1 <- osf_project_create("File Tests")


# tests -------------------------------------------------------------------

test_that("non-existent file is detected", {
  expect_error(osf_upload(p1, "non-existent-file"), "Can't find file")
})

test_that("file is uploaded to project root", {
  f1 <- osf_upload(p1, txt.file)
  expect_s3_class(f1, "osf_tbl_file")
})

test_that("upload fails if the file already exists", {
  expect_error(osf_upload(p1, txt.file), "Cannot complete action: file or folder")
})

test_that("upload can overwrite existing files", {
  f2 <- osf_upload(p1, txt.file, overwrite = TRUE)
  expect_s3_class(f2, "osf_tbl_file")
})
