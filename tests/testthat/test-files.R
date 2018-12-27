context("Uploading")

# setup -------------------------------------------------------------------
txt.file <- file.path(tempdir(), "osfr-test-file.txt")
writeLines("Lorem ipsum dolor sit amet, consectetur", txt.file)

p1 <- osf_project_create("File Tests")


# tests -------------------------------------------------------------------
test_that("non-existent file is detected", {
  expect_error(osf_upload(p1, "non-existent-file"), "Can't find file")
})

f1 <- osf_upload(p1, txt.file)

test_that("file is uploaded to project root", {
  expect_s3_class(f1, "osf_tbl_file")
  expect_match(f1$name, basename(txt.file))
})

test_that("uploaded file can be retrieved", {
  f2 <- osf_retrieve_file(as_id(f1))
  expect_identical(f1, f2)
})

test_that("upload fails if the file already exists", {
  expect_error(osf_upload(p1, txt.file), "Cannot complete action: file or folder")
})

writeLines("Lorem ipsum dolor sit amet, consectetur, ea duo posse", txt.file)

test_that("upload can overwrite existing files", {
  f1 <- osf_upload(p1, txt.file, overwrite = TRUE)
  expect_equal(f1$meta[[1]]$attributes$current_version, 2)
  expect_s3_class(f1, "osf_tbl_file")
})


# cleanup -----------------------------------------------------------------
osf_project_delete(p1, recursive = TRUE)
