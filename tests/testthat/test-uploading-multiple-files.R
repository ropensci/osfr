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

test_that("uploading does not recurse by default", {
  skip_if_no_pat()

  c1 <- osf_create_component(p1, "recurse = off")
  out <- osf_upload(c1, multidir)
  expect_equal(out$name, basename(multidir))

  # verify files and subdirectories were uploaded/created
  expect_equal(osf_ls_files(out)$name, list.files(multidir))

  # but subdirectories should be empty
  subdir1 <- osf_ls_files(out, path = "subdir1")
  expect_equal(nrow(subdir1), 0)
  subdir2 <- osf_ls_files(out, path = "subdir2")
  expect_equal(nrow(subdir2), 0)
})

test_that("recursive uploading works", {
  skip_if_no_pat()

  c2 <- osf_create_component(p1, "recurse = on")
  out <- osf_upload(c2, multidir, recurse = TRUE)
  expect_equal(out$name, basename(multidir))

  # subdirectory contents were uploaded
  subdir1 <- osf_ls_files(out, path = "subdir1")
  expect_equal(subdir1$name, c("d.txt", "subdir1_1"))

  subdir1_1 <- osf_ls_files(out, path = "subdir1/subdir1_1")
  expect_equal(subdir1_1$name, c("e.txt", "f.txt"))

  subdir2 <- osf_ls_files(out, path = "subdir2")
  expect_equal(subdir2$name, "g.txt")
})
