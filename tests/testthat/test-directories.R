context("Directories")

# setup -------------------------------------------------------------------
vcr_config("cassettes/directories")

setup({
  if (has_pat()) {
    vcr::use_cassette("create-p1", record = record, {
      p1 <<- osf_create_project(title = "osfr-test-directories")
    })
  }
})


# tests -------------------------------------------------------------------
test_that("empty project/folder returns a osf_tbl_file with 0 rows", {
  skip_if_no_pat()
  vcr::use_cassette("list-empty-p1", record = record, {
    out <- osf_ls_files(p1)
  })

  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 0)
})

test_that("listing a non-existent folder errors", {
  skip_if_no_pat()
  expect_error(
    vcr::use_cassette("list-nonexistent-dir", record = record, {
      osf_ls_files(p1, path = "nonexistent")
    }),
    "Can't find directory"
  )
})

test_that("create a top-level directory", {
  skip_if_no_pat()
  vcr::use_cassette("create-dir1", record = record, {
    d1 <- osf_mkdir(p1, path = "dir1")
  })

  expect_s3_class(d1, "osf_tbl_file")
  expect_equal(d1$name, "dir1")
})

test_that("list a top-level directory", {
  skip_if_no_pat()
  vcr::use_cassette("list-p1-with-one-dir", record = record, {
    out <- osf_ls_files(p1)
  })

  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 1)
  expect_equal(out$name, "dir1")
})


test_that("create a subdirectory within an existing directory", {
  skip_if_no_pat()
  vcr::use_cassette("create-subdir-dir11", record = record, {
    d11 <- osf_mkdir(p1, path = "dir1/dir11")
  })

  expect_s3_class(d11, "osf_tbl_file")
  expect_equal(d11$name, "dir11")
})

test_that("list within a subdirectory", {
  skip_if_no_pat()
  vcr::use_cassette("list-within-subdir-dir1", record = record, {
    out <- osf_ls_files(p1, path = "dir1")
  })

  expect_equal(nrow(out), 1)
  expect_equal(out$name, "dir11")
})

test_that("create a subdirectory within a non-existent parent directory", {
  skip_if_no_pat()
  vcr::use_cassette("create-subdir-dir21", record = record, {
    d21 <- osf_mkdir(p1, path = "dir2/dir21")
  })

  expect_s3_class(d21, "osf_tbl_file")
  expect_equal(d21$name, "dir21")

  d2_attrs <- d21$meta[[1]]$attributes
  expect_equal(d2_attrs$materialized_path, "/dir2/dir21/")
})

test_that("'path' isn't confused by dir names with a shared substring (#95)", {
  skip_if_no_pat()
  vcr::use_cassette("create-dir", record = record, {
    d3 <- osf_mkdir(p1, path = "dir")
  })

  vcr::use_cassette("list-dir", record = record, {
    out <- osf_ls_files(p1, path = "dir")
  })
  expect_equal(nrow(out), 0)
})

test_that("directory names can start with a dot", {
  skip_if_no_pat()
  vcr::use_cassette("create-dir-with-dot-prefix", record = record, {
    dotdir <- osf_mkdir(p1, path = ".dir")
  })

  expect_s3_class(dotdir, "osf_tbl_file")
  expect_equal(dotdir$name, ".dir")
})


teardown({
  if (has_pat()) {
    vcr::use_cassette("delete-p1", record = record, {
      osf_rm(p1, recurse = TRUE, check = FALSE)
    })
  }
})
