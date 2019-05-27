context("Uploading multiple files")


# setup -------------------------------------------------------------------

setup({
  # copy directory for testing multifile uploads to pwd
  multidir <<- fs::dir_copy(
    file.path(rprojroot::find_testthat_root_file(), "test-files", "uploads"),
    "."
  )

  if (has_pat()) {
    p1 <<- osf_create_project(title = "osfr-multifile-upload-test")
  }
})

teardown({
  fs::dir_delete(multidir)
  if (has_pat()) {
    osf_rm(p1, recursive = TRUE, check = FALSE)
  }
})


# tests -------------------------------------------------------------------

test_that("multiple files can be uploaded", {
  skip_if_no_pat()

  infiles <- fs::dir_ls(multidir, type = "file")
  out <- osf_upload(p1, infiles)
  expect_s3_class(out, "osf_tbl_file")
  expect_equal(out$name, basename(infiles))
})

test_that("a directory can be uploaded", {
  skip_if_no_pat()

  indir <- file.path(multidir, "subdir1", "subdir1_1")
  out <- osf_upload(p1, indir)
  expect_s3_class(out, "osf_tbl_file")
  expect_equal(out$name, basename(indir))

  # verify files within the directory were uploaded
  expect_equal(osf_ls_files(out)$name, list.files(indir))
})
