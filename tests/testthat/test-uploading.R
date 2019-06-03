context("Uploading files")


# setup -------------------------------------------------------------------
infile <- tempfile("osfr-local-file-", fileext = ".txt")

setup({
  writeLines("Lorem ipsum dolor sit amet, consectetur", infile)

  if (has_pat()) {
    p1 <<- osf_create_project(title = "osfr-test-files-1")
    p2 <<- osf_create_project(title = "osfr-test-files-2")
  }
})

teardown({
  if (has_pat()) {
    osf_rm(p1, recursive = TRUE, check = FALSE)
    osf_rm(p2, recursive = TRUE, check = FALSE)
  }
})


# tests -------------------------------------------------------------------
test_that("non-existent file is detected", {
  expect_error(osf_upload(p1, "non-existent-file"), "Can't find following")
})

test_that("file is uploaded to project root", {
  skip_if_no_pat()

  expect_message(
    f1 <<- osf_upload(p1, infile, verbose = TRUE),
    sprintf("Uploaded new file %s to OSF", basename(infile))
  )

  expect_s3_class(f1, "osf_tbl_file")
  expect_match(f1$name, basename(infile))
})

test_that("uploaded file can be retrieved", {
  skip_if_no_pat()

  f2 <- osf_retrieve_file(as_id(f1))
  expect_identical(f1, f2)
})

test_that("user is warned if a file already exists", {
  skip_if_no_pat()
  expect_warning(
    out <- osf_upload(p1, infile),
    sprintf("Local file '%s' was NOT uploaded", basename(infile))
  )
  # existing OSF file is returned
  expect_identical(out, f1)
})

test_that("upload can overwrite existing files", {
  writeLines("Lorem ipsum dolor sit amet, consectetur, ea duo posse", infile)
  skip_if_no_pat()

  expect_message(
    f1 <<- osf_upload(p1, infile, overwrite = TRUE, verbose = TRUE),
    sprintf("Uploaded new version of '%s' to OSF", basename(infile))
  )

  expect_equal(f1$meta[[1]]$attributes$current_version, 2)
  expect_s3_class(f1, "osf_tbl_file")
})

test_that("file can be uploaded to a directory", {
  skip_if_no_pat()

  d1 <<- osf_mkdir(p1, "data")
  expect_message(
    f2 <<- osf_upload(d1, infile, verbose = TRUE),
    sprintf("Uploaded new file %s to OSF", basename(infile))
  )
  expect_s3_class(f2, "osf_tbl_file")
})

test_that("attempting to list an osf_tbl_file with a file errors", {
  skip_if_no_pat()
  expect_error(osf_ls_files(f1), "Listing an `osf_tbl_file` requires a dir")
})
