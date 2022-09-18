context("Uploading files")


# setup -------------------------------------------------------------------

vcr::vcr_configure(
  dir = cassette_dir("uploading-single-files")
)

setup({
  testdir <<- fs::dir_create(".osfr-tests")
  infile <<- fs::path(testdir, "osfr-local-file.txt")
  brio::writeLines("Lorem ipsum dolor sit amet, consectetur", infile)

  if (has_pat()) {
    vcr::use_cassette("create-p1", {
      p1 <<- osf_create_project(title = "osfr-test-files-1")
      d1 <<- osf_mkdir(p1, "data")
    })
  }
})

teardown({
  fs::dir_delete(testdir)
  if (has_pat()) {
    vcr::use_cassette("delete-p1", {
      osf_rm(p1, recurse = TRUE, check = FALSE)
    })
  }
})


# tests -------------------------------------------------------------------
test_that("nonexistent file is detected", {
  skip_if_no_pat()
  expect_error(osf_upload(p1, "nonexistent-file"), "Can't find following")
})

test_that("file is uploaded to project root", {
  skip_if_no_pat()

  expect_message(
    vcr::use_cassette("upload-infile", {
      f1 <<- osf_upload(p1, infile, verbose = TRUE)
    }),
    sprintf("Uploaded new file '%s' to OSF", basename(infile))
  )

  expect_s3_class(f1, "osf_tbl_file")
  expect_match(f1$name, basename(infile))
})

test_that("uploaded file can be retrieved", {
  skip_if_no_pat()

  vcr::use_cassette("retrieve-infile", {
    f2 <- osf_retrieve_file(as_id(f1))
  })
  expect_identical(f1, f2)
})

test_that("by default an error is thrown if a conflicting file exists", {
  skip_if_no_pat()
  expect_error(
    vcr::use_cassette("upload-conflict-error", {
      out <- osf_upload(p1, infile)
    }),
    sprintf("Can't upload file '%s'", basename(infile))
  )
})

test_that("a file can be overwritten when conflicts='overwrite'", {
  brio::writeLines(
    text = "Lorem ipsum dolor sit amet, consectetur, ea duo posse",
    con = infile
  )
  skip_if_no_pat()

  expect_message(
    vcr::use_cassette("upload-conflict-overwrite", {
      f1 <<- osf_upload(p1, infile, conflicts = "overwrite", verbose = TRUE)
    }),
    sprintf("Uploaded new version of '%s' to OSF", basename(infile))
  )

  version <- get_meta(f1, "attributes", "current_version")
  expect_equal(version, 2)
})


test_that("a file can be uploaded to a directory", {
  skip_if_no_pat()
  vcr::use_cassette("upload-to-directory", {
    f2 <- osf_upload(d1, infile)
  })
  expect_s3_class(f2, "osf_tbl_file")
})

test_that("conflicting files can be skipped when uploading to a dir", {
  skip_if_no_pat()
  expect_message(
    vcr::use_cassette("upload-conflict-skip", {
      f2 <- osf_upload(d1, infile, conflicts = "skip")
    }),
    "Skipped 1 file\\(s\\) to avoid overwriting OSF copies"
  )
  expect_s3_class(f2, "osf_tbl_file")
})

test_that("conflicting files can be overwritten when uploading to a dir", {
  skip_if_no_pat()
  on.exit(vcr::eject_cassette())
  brio::writeLines(
    text = "Lorem ipsum dolor sit amet, consectetur, ea trio posse",
    con = infile
  )

  vcr::insert_cassette("upload-conflict-overwrite-within-directory")
  expect_silent(
    f2 <- osf_upload(d1, infile, conflicts = "overwrite")
  )

  # file is now on version 2
  expect_equal(get_meta(f2, "attributes", "current_version"), 2)
})

test_that("attempting to list an osf_tbl_file with a file errors", {
  skip_if_no_pat()
  expect_error(osf_ls_files(f1), "Listing an `osf_tbl_file` requires a dir")
})

test_that("an empty directory can be uploaded", {
  skip_if_no_pat()
  on.exit(fs::dir_delete(d))
  d <- fs::dir_create("empty")

  vcr::use_cassette("upload-empty-directory", {
    out <- osf_upload(p1, path = d)
  })
  expect_s3_class(out, "osf_tbl_file")
})
