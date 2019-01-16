context("Uploading")

# setup -------------------------------------------------------------------
txt.file <- file.path(tempdir(), "osfr-test-file.txt")
writeLines("Lorem ipsum dolor sit amet, consectetur", txt.file)

p1 <- osf_create_project("File Tests")


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
  expect_error(osf_upload(p1, txt.file), "File already exists at destination")
})

writeLines("Lorem ipsum dolor sit amet, consectetur, ea duo posse", txt.file)

test_that("upload can overwrite existing files", {
  f1 <- osf_upload(p1, txt.file, overwrite = TRUE)
  expect_equal(f1$meta[[1]]$attributes$current_version, 2)
  expect_s3_class(f1, "osf_tbl_file")
})

test_that("user is warned that path info is removed from upload name", {
  expect_warning(
    osf_upload(p1, txt.file, name = "path/file.txt"),
    "Removing path information"
  )
})

d1 <- osf_mkdir(p1, "data")
f2 <- osf_upload(d1, txt.file)

test_that("file can be uploaded to a directory", {
  expect_s3_class(f2, "osf_tbl_file")
})

test_that("attempting to list an osf_tbl_file with a file errors", {
  expect_error(osf_ls_files(f1), "Listing an `osf_tbl_file` requires a dir")
})


context("Downloading")

outfile <- tempfile(fileext = ".txt")

test_that("a file can be downloaded from a project", {
  out <- osf_download(f1, path = outfile)
  expect_s3_class(out, "osf_tbl_file")
  expect_identical(out$local_path, outfile)
  expect_true(file.exists(outfile))
})

test_that("an existing file won't be overwritten", {
  expect_error(osf_download(f1, path = outfile), "A file exists at the specified")
  expect_s3_class(osf_download(f1, path = outfile, overwrite = TRUE), "osf_tbl_file")
})

test_that("a non-existant path throws an error", {
  expect_error(
    osf_download(f1, path = "ddd/test.txt"),
    "The directory specified in `path` does not exist.")
})

test_that("a file can be downloaded from a directory", {
  outfile <- tempfile(fileext = ".txt")
  out <- osf_download(f2, path = outfile)
  expect_s3_class(out, "osf_tbl_file")
  expect_identical(out$local_path, outfile)
  expect_true(file.exists(outfile))
})

test_that("a directory can be downloaded as a zip file", {
  outfile <- tempfile(fileext = ".zip")
  out <- osf_download(d1, path = outfile)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(outfile))

  expect_match(
    unzip(outfile, list = TRUE)$Name[1],
    basename(txt.file)
  )
})


context("Deleting files")

test_that("a single file can be deleted", {
  expect_true(osf_rm(f1, check = FALSE))
})

test_that("an empty directory can be deleted", {
  d2 <- osf_mkdir(p1, "empty")
  expect_true(osf_rm(d2, check = FALSE))
})

test_that("a non-empty directory can be deleted", {
  expect_true(osf_rm(d1, check = FALSE))
})


# cleanup -----------------------------------------------------------------
osf_rm(p1, check = FALSE)
