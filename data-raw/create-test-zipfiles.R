# Create zipfiles to represent OSF directories in unzip tests

testdir <- "tests/testthat/test-files"
dir.create(testdir, showWarnings = FALSE)

zipdir1 <- "test-dir1"
file1a <- "file1a.txt"
file1b <- "file1b.txt"

writeLines("foo", file1a)
writeLines("bar", file1b)

zipfile1 <- file.path(testdir, paste0(zipdir1, ".zip"))
zip(zipfile1, files = c(file1a, file1b))

zipdir2 <- "test-dir2"
file2a <- "file2a.txt"
file2b <- "file2b.txt"

writeLines("fizz", file2a)
writeLines("buzz", file2b)

zipfile2 <- file.path(testdir, paste0(zipdir2, ".zip"))
zip(zipfile2, files = c(file2a, file2b))

unlink(c(file1a, file1b, file2a, file2b))
