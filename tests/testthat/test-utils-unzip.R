context("Unzipping")


# setup -------------------------------------------------------------------
zipdir1 <- "text-files1"
zipdir2 <- "text-files2"

zipfile1 <- paste0(zipdir1, ".zip")
file1a <- file.path(zipdir1, "file1a.txt")
file1b <- file.path(zipdir1, "file1b.txt")

zipfile2 <- paste0(zipdir2, ".zip")
file2a <- file.path(zipdir2, "file2a.txt")
file2b <- file.path(zipdir2, "file2b.txt")

setup({
  dir.create(zipdir1)
  writeLines("foo", file1a)
  writeLines("bar", file1b)
  zip(zipfile1, dir(zipdir1, full.names = TRUE))
  unlink(zipdir1, recursive = TRUE)

  dir.create(zipdir2)
  writeLines("fizz", file2a)
  writeLines("buzz", file2b)
  zip(zipfile2, dir(zipdir2, full.names = TRUE))
  unlink(zipdir2, recursive = TRUE)
})


teardown({
  unlink(zipdir1, recursive = TRUE)
  unlink(zipdir2, recursive = TRUE)
})


# tests -------------------------------------------------------------------

test_that("can unzip a single zipfile", {
  expect_false(dir.exists(zipdir1))

  out <- unzip_files(zipfile1)
  expect_true(dir.exists(zipdir1))
  expect_true(all(file.exists(out[[1]])))
})

test_that("can unzip multiple files", {
  zip(zipfile1, dir(zipdir1, full.names = TRUE))
  out <- unzip_files(c(zipfile1, zipfile2), overwrite = TRUE)

  # deleted the zip files
  expect_false(any(file.exists(names(out))))

  # zip file contents exist
  expect_true(all(file.exists(unlist(out))))
})

test_that("existing files are not overwritten", {

  # recreate zipfiles
  zip(zipfile1, dir(zipdir1, full.names = TRUE))
  zip(zipfile2, dir(zipdir2, full.names = TRUE))

  # delete first file and modify the second
  unlink(file1a)
  writeLines("foo", file1b)

  # delete second directory
  unlink(zipdir2, recursive = TRUE)

  out <- unzip_files(c(zipfile1, zipfile2))

  expect_true(file.exists(file1a))
  expect_true(file.exists(file1b))
  expect_true(file.exists(file2a))
  expect_true(file.exists(file2b))

  # file1b's modification persists
  expect_match(readLines(file1b), "foo")
})
