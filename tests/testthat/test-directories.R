# setup -------------------------------------------------------------------
vcr::vcr_configure(
  dir = cassette_dir("directories")
)

vcr::use_cassette("create-p1", {
  p1 <- osf_create_project(title = "osfr-test-directories")
})

withr::defer(
  if (has_pat()) try(osf_rm(p1, recurse = TRUE, check = FALSE), silent = TRUE),
  testthat::teardown_env()
)

# tests -------------------------------------------------------------------
test_that("empty project/folder returns an empty osf_tbl_file", {
  vcr::use_cassette("list-empty-p1", {
    out <- osf_ls_files(p1)
  })

  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 0)
})

test_that("listing a non-existent folder errors", {
  expect_error(
    vcr::use_cassette("list-nonexistent-dir", {
      osf_ls_files(p1, path = "nonexistent")
    }),
    "Can't find directory"
  )
})

test_that("create a top-level directory", {
  vcr::use_cassette("create-dir1", {
    d1 <- osf_mkdir(p1, path = "dir1")
  })

  expect_s3_class(d1, "osf_tbl_file")
  expect_equal(d1$name, "dir1")
})

test_that("list a top-level directory", {
  vcr::use_cassette("list-p1-with-one-dir", {
    out <- osf_ls_files(p1)
  })

  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 1)
  expect_equal(out$name, "dir1")
})

test_that("create a subdirectory within an existing directory", {
  vcr::use_cassette("create-subdir-dir11", {
    d11 <- osf_mkdir(p1, path = "dir1/dir11")
  })

  expect_s3_class(d11, "osf_tbl_file")
  expect_equal(d11$name, "dir11")
})

test_that("list within a subdirectory", {
  vcr::use_cassette("list-within-subdir-dir1", {
    out <- osf_ls_files(p1, path = "dir1")
  })

  expect_equal(nrow(out), 1)
  expect_equal(out$name, "dir11")
})

test_that("create a subdirectory within a non-existent parent directory", {
  vcr::use_cassette("create-subdir-dir21", {
    d21 <- osf_mkdir(p1, path = "dir2/dir21")
  })

  expect_s3_class(d21, "osf_tbl_file")
  expect_equal(d21$name, "dir21")

  d2_attrs <- d21$meta[[1]]$attributes
  expect_equal(d2_attrs$materialized_path, "/dir2/dir21/")
})

test_that("'path' isn't confused by dir names with a shared substring (#95)", {
  vcr::use_cassette("create-dir", {
    d3 <- osf_mkdir(p1, path = "dir")
  })

  vcr::use_cassette("list-dir", {
    out <- osf_ls_files(p1, path = "dir")
  })
  expect_equal(nrow(out), 0)
})

test_that("directory names can start with a dot", {
  vcr::use_cassette("create-dir-with-dot-prefix", {
    dotdir <- osf_mkdir(p1, path = ".dir")
  })

  expect_s3_class(dotdir, "osf_tbl_file")
  expect_equal(dotdir$name, ".dir")
})
