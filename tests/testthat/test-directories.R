context("Directories")

# setup -------------------------------------------------------------------
setup({
  vcr::insert_cassette("test-directories", record = "new_episodes")
  if (has_pat()) {
    p1 <<- osf_create_project(title = "osfr-test-directories")
  }
})

teardown({
  if (has_pat()) {
    osf_rm(p1, recurse = TRUE, check = FALSE)
  }
  vcr::eject_cassette("test-directories")
})


# tests -------------------------------------------------------------------
test_that("empty project/folder returns a osf_tbl_file with 0 rows", {
  skip_if_no_pat()

  out <- osf_ls_files(p1)
  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 0)
})

test_that("listing a non-existent folder errors", {
  vcr::insert_cassette("test-directories-nonexistent"), record = "new_episodes")
  skip_if_no_pat()
  expect_error(osf_ls_files(p1, path = "nonexistent"), "Can't find directory")
  vcr::eject_cassette()
})

test_that("create a top-level directory", {
  skip_if_no_pat()

  d1 <- osf_mkdir(p1, path = "dir1")
  expect_s3_class(d1, "osf_tbl_file")
  expect_equal(d1$name, "dir1")
})

test_that("list a top-level directory", {
  skip_if_no_pat()

  p1_nonempty <- p1
  out <- osf_ls_files(p1_nonempty)
  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 1)
  expect_equal(out$name, "dir1")
})


test_that("create a subdirectory within an existing directory", {
  skip_if_no_pat()

  d11 <- osf_mkdir(p1, path = "dir1/dir11")
  expect_s3_class(d11, "osf_tbl_file")
  expect_equal(d11$name, "dir11")
})

test_that("list a subdirectory", {
  skip_if_no_pat()

  p1_withsubdir <- p1
  out <- osf_ls_files(p1_withsubdir)
  expect_equal(nrow(out), 1)
  expect_equal(out$name, "dir1")

  out <- osf_ls_files(p1_withsubdir, path = "dir1")
  expect_equal(nrow(out), 1)
  expect_equal(out$name, "dir11")
})

test_that("create a subdirectory within a non-existent parent directory", {
  skip_if_no_pat()

  d21 <- osf_mkdir(p1, path = "dir2/dir21")
  expect_s3_class(d21, "osf_tbl_file")
  expect_equal(d21$name, "dir21")

  d2_attrs <- d21$meta[[1]]$attributes
  expect_equal(d2_attrs$materialized_path, "/dir2/dir21/")
})

test_that("'path' isn't confused by dir names with a shared substring (#95)", {
  skip_if_no_pat()

  d3 <- osf_mkdir(p1, path = "dir")
  expect_silent(osf_ls_files(p1, path = "dir"))
})

test_that("directory names can start with a dot", {
  skip_if_no_pat()

  dotdir <- osf_mkdir(p1, path = ".dir")
  expect_s3_class(dotdir, "osf_tbl_file")
  expect_equal(dotdir$name, ".dir")
})
