context("Unzipping")


# setup -------------------------------------------------------------------

# create temporary copies of zipfiles for testing
copy_test_zipfiles <<- function(x) {
  zipfile <- rprojroot::find_testthat_root_file(file.path("test-files", x))
  fs::file_copy(zipfile, tempdir())
}


setup({
  zipfile1 <<- copy_test_zipfiles("test-dir1.zip")
  zipfile2 <<- copy_test_zipfiles("test-dir2.zip")

  zipdir1 <<- fs::path_ext_remove(zipfile1)
  zipdir2 <<- fs::path_ext_remove(zipfile2)
})


teardown({
  unlink(c(zipdir1, zipdir2), recursive = TRUE)
})


# tests -------------------------------------------------------------------

test_that("can unzip a single zipfile", {
  expect_false(dir.exists(zipdir1))

  out <- unzip_files(zipfile1)
  expect_true(dir.exists(zipdir1))
  expect_true(all(file.exists(out[[1]])))

  unlink(zipdir1, recursive = TRUE)
  copy_test_zipfiles("test-dir1.zip")
})

test_that("can unzip multiple files", {
  out <- unzip_files(c(zipfile1, zipfile2))

  # the zip files were deleted
  expect_false(any(file.exists(names(out))))

  # the zip file content exist
  expect_true(all(file.exists(unlist(out))))

  unlink(c(zipdir1, zipdir2), recursive = TRUE)
  copy_test_zipfiles(c("test-dir1.zip", "test-dir2.zip"))
})

test_that("existing files are not overwritten", {

  # delete first zip file and modify the second
  out <- unzip_files(zipfile1)
  unlink(out[[1]][1])
  writeLines("foo", out[[1]][2])

  # recreate the zipfile
  copy_test_zipfiles("test-dir1.zip")
  out <- unzip_files(c(zipfile1, zipfile2))

  # both files were unzipped
  unzipped_files <- list.files(zipdir1, full.names = TRUE)
  expect_equal(length(unzipped_files), 2)

  # file1b's modification persists
  expect_match(readLines(unzipped_files[1]), "foo")
})
