context("Moving files")


# setup -------------------------------------------------------------------
infile <- tempfile("osfr-local-file-", fileext = ".txt")

setup({
  writeLines("Lorem ipsum dolor sit amet, consectetur", infile)

  if (has_pat()) {
    p1 <<- osf_create_project(title = "osfr-test-files-1")
    p2 <<- osf_create_project(title = "osfr-test-files-2")
    d1 <<- osf_mkdir(p1, "d1")
    d3 <<- osf_mkdir(p2, "d3")
    f1 <<- osf_upload(p1, infile)
  }
})

teardown({
  if (has_pat()) {
    osf_rm(p1, recursive = TRUE, check = FALSE)
    osf_rm(p2, recursive = TRUE, check = FALSE)
  }
})


# tests -------------------------------------------------------------------
test_that("a file can be moved from node to subdirectory and back", {
  skip_if_no_pat()
  f1 <- osf_mv(f1, d1)
  expect_match(
    get_meta(f1, "attributes", "materialized_path"),
    file.path(d1$name, basename(infile))
  )

  f1 <- osf_mv(f1, p1)
  expect_equal(
    get_meta(f1, "attributes", "materialized_path"),
    paste0("/", basename(infile))
  )
})

test_that("moving respects overwrite argument", {
  skip_if_no_pat()
  f2 <- osf_upload(d1, infile)
  expect_error(osf_mv(f1, d1), "Cannot complete action: file or folder")

  f1 <- osf_mv(f1, d1, overwrite = TRUE)
  expect_s3_class(f1, "osf_tbl_file")

  expect_match(
    get_meta(f1, "attributes", "materialized_path"),
    file.path(d1$name, f1$name)
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
  child <- osf_mkdir(parent, "child")
  expect_error(
    osf_mv(parent, child),
    "Can't move a parent directory into its child"
  )
})


context("Copying files")

test_that("a file can copy to new directory", {
  skip_if_no_pat()
  f1 <- osf_refresh(f1)
  f_copy <- osf_cp(f1, d3)
  expect_identical(get_parent_id(f_copy), get_parent_id(f1))
  osf_rm(f_copy, check = FALSE)
})

test_that("a file cannot copy to same location with either overwrite option", {
  skip_if_no_pat()
  f1 <- osf_refresh(f1)
  expect_error(
    osf_cp(f1, p2, overwrite = FALSE),
    "Cannot complete action:"
  )

  expect_error(
    osf_cp(f1, p2, overwrite = TRUE),
    "Unable to move or copy"
  )
})

test_that("copy will respect overwrite values when copying to a new location", {
  skip_if_no_pat()
  f1 <- osf_refresh(f1)
  f_copy <- osf_cp(f1, d3)
  expect_error(
    osf_cp(f_copy, d3, overwrite = FALSE),
    "Cannot complete action:"
  )

  f1 <- osf_refresh(f1)
  f_copy_2 <- osf_cp(f1, d3, overwrite = TRUE)
  expect_identical(get_parent_id(f_copy_2), get_parent_id(f1))
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
