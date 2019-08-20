context("Uploading multiple files")


# setup -------------------------------------------------------------------

setup({
  # copy directory for testing multifile uploads to pwd
  multidir <<- fs::dir_copy(
    file.path(rprojroot::find_testthat_root_file(), "test-files", "uploads"),
    ".osfr-tests"
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

  c1_dir <- osf_create_component(p1, "dir-upload")
  out <- osf_upload(c1_dir, multidir)
  expect_s3_class(out, "osf_tbl_file")
  expect_match(out$name, multidir)

  # files within the directory were uploaded
  expect_equal(
    osf_ls_files(out, type = "file")$name,
    basename(fs::dir_ls(multidir, type = "file"))
  )

  # subdirectories within the directory were created
  subdirs <- basename(fs::dir_ls(multidir, type = "directory"))
  expect_equal(
    osf_ls_files(out, type = "folder")$name,
    subdirs
  )

  # subdirectories are empty with recurse=FALSE
  subdir1 <- osf_ls_files(out, path = subdirs[1])
  expect_equal(nrow(subdir1), 0)
})

test_that("a subdirectory can be uploaded", {
  skip_if_no_pat()

  # upload the subdirectory with no children
  c2_subdir <- osf_create_component(p1, "subdir-upload")

  indir <- file.path(multidir, "subdir1", "subdir1_1")
  out <- osf_upload(c2_subdir, indir)

  expect_s3_class(out, "osf_tbl_file")
  expect_match(out$name, basename(indir))

  # verify leaf directory was created in the project root
  expect_match(
    osf_ls_files(c2_subdir, type = "folder")$name,
    basename(indir)
  )

  # verify files within the directory were uploaded
  expect_equal(osf_ls_files(out)$name, list.files(indir))
})


test_that("recurse argument respects specified levels", {
  skip_if_no_pat()

  c3_recurse <- osf_create_component(p1, "recurse=1")
  out <- osf_upload(c3_recurse, path = multidir, recurse = 1, verbose = TRUE)

  sd1_files <- osf_ls_files(out, path = "subdir1")
  expect_equal(
    sd1_files$name,
    dir(file.path(multidir, "subdir1"))
  )

  sd2_files <- osf_ls_files(out, path = "subdir2")
  expect_equal(
    sd2_files$name,
    dir(file.path(multidir, "subdir2"))
  )

  sd1_1_files <- osf_ls_files(out, path = "subdir1/subdir1_1")
  expect_equal(nrow(sd1_1_files), 0)
})


test_that("recurse=TRUE uploads the entire OSF directory structure", {
  skip_if_no_pat()

  c4_recurse <- osf_create_component(p1, "recurse=TRUE")
  out <- osf_upload(c4_recurse, path = multidir, recurse = TRUE)

  sd1_1_files <- osf_ls_files(out, path = "subdir1/subdir1_1")
  expect_equal(
    sd1_1_files$name,
    dir(file.path(multidir, "subdir1/subdir1_1"))
  )
})

