context("Uploading")


# setup -------------------------------------------------------------------
infile <- tempfile("osfr-local-file-", fileext = ".txt")
outfile <- basename(infile)

setup({
  writeLines("Lorem ipsum dolor sit amet, consectetur", infile)
  if (has_pat()) {
    p1 <<- osf_create_project(title = "osfr-test-files-1")
    p2 <<- osf_create_project(title = "osfr-test-files-2")
  }
})

teardown({
  unlink(outfile)

  if (has_pat()) {
    osf_rm(p1, recursive = TRUE, check = FALSE)
    osf_rm(p2, recursive = TRUE, check = FALSE)
  }
})


# tests -------------------------------------------------------------------
test_that("non-existent file is detected", {
  expect_error(osf_upload(p1, "non-existent-file"), "Can't find file")
})

test_that("file is uploaded to project root", {
  skip_if_no_pat()

  f1 <<- osf_upload(p1, infile)
  expect_s3_class(f1, "osf_tbl_file")
  expect_match(f1$name, outfile)
})

test_that("uploaded file can be retrieved", {
  skip_if_no_pat()

  f2 <- osf_retrieve_file(as_id(f1))
  expect_identical(f1, f2)
})

test_that("upload fails if the file already exists", {
  skip_if_no_pat()
  expect_error(osf_upload(p1, infile), "File already exists at destination")
})


test_that("upload can overwrite existing files", {
  skip_if_no_pat()

  writeLines("Lorem ipsum dolor sit amet, consectetur, ea duo posse", infile)
  skip_if_no_pat()

  f1 <- osf_upload(p1, infile, overwrite = TRUE)
  expect_equal(f1$meta[[1]]$attributes$current_version, 2)
  expect_s3_class(f1, "osf_tbl_file")
})

test_that("user is warned that path info is removed from upload name", {
  skip_if_no_pat()

  expect_warning(
    osf_upload(p1, infile, name = "path/file.txt"),
    "Removing path information"
  )
})


test_that("file can be uploaded to a directory", {
  skip_if_no_pat()

  d1 <<- osf_mkdir(p1, "data")
  f2 <<- osf_upload(d1, infile)
  expect_s3_class(f2, "osf_tbl_file")
})

test_that("attempting to list an osf_tbl_file with a file errors", {
  skip_if_no_pat()
  expect_error(osf_ls_files(f1), "Listing an `osf_tbl_file` requires a dir")
})

test_that("messages are printed with `verbose` enabled", {
  skip_if_no_pat()

  infile2 <- sub(".txt", "_2.txt", infile)
  dev.null <- file.copy(infile, infile2)

  # uploading to a node
  expect_message(
    osf_upload(p1, infile2, verbose = TRUE),
    sprintf("Uploaded %s to OSF", basename(infile2))
  )

  # uploading to a directory
  expect_message(
    osf_upload(d1, infile2, verbose = TRUE),
    sprintf("Uploaded %s to OSF", basename(infile2))
  )

  # updating node file
  expect_message(
    osf_upload(p1, infile2, overwrite = TRUE, verbose = TRUE),
    sprintf("Uploaded new version of %s to OSF", basename(infile2))
  )

  # updating directory file
  expect_message(
    osf_upload(d1, infile2, overwrite = TRUE, verbose = TRUE),
    sprintf("Uploaded new version of %s to OSF", basename(infile2))
  )
})


context("Downloading")
test_that("a file can be downloaded from a project", {

  skip_if_no_pat()

  out <- osf_download(f1, path = outfile)
  expect_s3_class(out, "osf_tbl_file")
  expect_identical(out$local_path, outfile)
  expect_true(file.exists(outfile))
})

test_that("an existing file won't be overwritten", {
  skip_if_no_pat()

  expect_error(
    osf_download(f1, path = outfile),
    "A file exists at the specified"
  )
  expect_s3_class(
    osf_download(f1, path = outfile, overwrite = TRUE),
    "osf_tbl_file"
  )
})

test_that("a non-existant path throws an error", {
  skip_if_no_pat()

  expect_error(
    osf_download(f1, path = "ddd/test.txt"),
    "The directory specified in `path` does not exist.")
})

test_that("a file can be downloaded from a directory", {
  skip_if_no_pat()

  outfile <- tempfile(fileext = ".txt")
  out <- osf_download(f2, path = outfile)
  expect_s3_class(out, "osf_tbl_file")
  expect_identical(out$local_path, outfile)
  expect_true(file.exists(outfile))
})

test_that("a directory can be downloaded as a zip file", {
  skip_if_no_pat()

  d1_files <- osf_ls_files(d1, n_max = Inf)
  outfile <- tempfile(fileext = ".zip")

  out <- osf_download(d1, path = outfile)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(outfile))

  expect_equal(
    sort(unzip(outfile, list = TRUE)$Name),
    sort(d1_files$name)
  )
})


context("Moving/copying files")

test_that("moving to a destination with an existing file throws an error", {
  skip_if_no_pat()
  expect_error(osf_mv(f1, d1), "Cannot complete action: file or folder")
})

test_that("moving can overwrite an existing file", {
  skip_if_no_pat()

  f1 <- osf_mv(f1, d1, overwrite = TRUE)
  expect_s3_class(f1, "osf_tbl_file")

  expect_match(
    get_meta(f1, "attributes", "materialized_path"),
    file.path(d1$name, f1$name)
  )
})

test_that("moving destination can be the parent node", {
  skip_if_no_pat()

  f1 <- osf_mv(f1, p1)
  expect_s3_class(f1, "osf_tbl_file")

  expect_match(
    get_meta(f1, "attributes", "materialized_path"),
    paste0("/", f1$name)
  )
})

test_that("moving destination can be a different node", {
  skip_if_no_pat()

  f1 <- osf_mv(f1, p2)
  expect_match(get_parent_id(f1), as_id(p2))
})

test_that("directories can be moved to a sibling directory", {
  skip_if_no_pat()

  d2 <- osf_mkdir(p1, "d2")
  d1 <- osf_mv(d1, d2)
  expect_s3_class(f1, "osf_tbl_file")

  expect_match(
    paste0("/", file.path(d2$name, d1$name), "/"),
    get_meta(d1, "attributes", "materialized_path")
  )
})

test_that("moving a parent directory to a child directory errors", {
  skip_if_no_pat()

  parent <- osf_mkdir(p1, "parent")
  child <- osf_mkdir(p1, "parent/child")
  expect_error(
    osf_mv(parent, child),
    "Can't move a parent directory into its child"
  )
})


context("Deleting files")

test_that("a single file can be deleted", {
  skip_if_no_pat()

  f1 <- osf_refresh(f1)
  expect_true(osf_rm(f1, check = FALSE))
})

test_that("an empty directory can be deleted", {
  skip_if_no_pat()

  d2 <- osf_mkdir(p1, "d2")
  expect_true(osf_rm(d2, check = FALSE))
})

test_that("a non-empty directory can be deleted", {
  skip_if_no_pat()

  d3 <- osf_mkdir(p1, "d1/d2/d3")
  expect_true(osf_rm(d3, check = FALSE))
})
