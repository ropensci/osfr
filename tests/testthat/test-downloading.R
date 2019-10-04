context("Downloading files")


# setup -------------------------------------------------------------------
setup({
  # Retrieve public OSF project and components required for tests
  # (created using data-raw/create-test-project.R)
  if (on_test_server()) {
    guids <- get_guids()
    p1 <<- osf_retrieve_node(guids[, "p1"])
    c1 <<- osf_retrieve_node(guids[, "c1"])
    f1 <<- osf_retrieve_file(guids[, "f1"])
    d1 <<- osf_retrieve_file(guids[, "d1"])
    d2 <<- osf_retrieve_file(guids[, "d2"])
  }
  outdir <<- as.character(fs::dir_create(".osfr-tests"))
})


teardown({
  fs::dir_delete(outdir)
})


# tests -------------------------------------------------------------------

test_that("a file can be downloaded from a project", {
  skip_if_no_pat()

  out <- osf_download(f1, path = outdir)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(out$local_path))

  # verify the local_path wasn't returned as a relative path
  expect_equal(dirname(out$local_path), outdir)
})

test_that("by default an error is thrown if a conflicting file exists", {
  skip_if_no_pat()
  expect_error(
    osf_download(f1, path = outdir),
    paste0("Can't download file '", f1$name, "' from OSF.")
  )
})

test_that("a file can be overwritten when conflicts='overwrite'", {
  skip_if_no_pat()
  expect_silent(
    out <- osf_download(f1, path = outdir, conflict = "overwrite")
  )
  expect_s3_class(out, "osf_tbl_file")
})

test_that("a non-existant path throws an error", {
  skip_if_no_pat()
  expect_error(
    osf_download(f1, path = "ddd"),
    "`path` must point to an existing local directory."
  )
})

test_that("a file can be downloaded from an OSF directory", {
  skip_if_no_pat()
  f2 <- osf_ls_files(d1, pattern = "image-file-01.png")
  out <- osf_download(f2, path = outdir)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(out$local_path))
})

test_that("by default only top-level files are downloaded from an OSF directory", {
  skip_if_no_pat()

  out <- osf_download(d2, path = outdir)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(out$local_path))

  d2_files <- osf_ls_files(d2, type = "file")
  expect_equal(
    dir(file.path(outdir, d2$name), recursive = TRUE),
    d2_files$name
  )
  fs::dir_delete(file.path(outdir, d2$name))
})

test_that("recurse argument respects specified levels", {
  skip_if_no_pat()

  out <- osf_download(d2, path = outdir, recurse = 2)
  d01_files <- osf_ls_files(d2, path = "d01", type = "file")

  expect_equal(
    d01_files$name,
    dir(file.path(outdir, d2$name, "d01"))
  )

  expect_length(
    dir(file.path(outdir, d2$name), recursive = TRUE),
    4
  )

  fs::dir_delete(file.path(outdir, d2$name))
})

test_that("recurse=TRUE downloads the entire OSF directory structure", {
  skip_if_no_pat()
  out <- osf_download(d2, path = outdir, recurse = TRUE)
  d03_files <- osf_ls_files(d2, path = "d01/d02/d03", type = "file")

  expect_equal(
    d03_files$name,
    dir(file.path(outdir, d2$name, "d01/d02/d03"))
  )
  fs::dir_delete(file.path(outdir, d2$name))
})

test_that("conflicting files are skipped or overwritten", {
  skip_if_no_pat()
  out <- osf_download(d2, path = outdir)

# overwrite contents of first file and delete the second file
  top_files <- dir(file.path(outdir, d2$name), full.names = TRUE)
  f1_text <- readLines(top_files[1])
  writeLines("foo", con = top_files[1])
  fs::file_delete(top_files[2])

  out <- osf_download(d2, path = outdir, conflicts = "skip")

  # modified file was not overwritten
  expect_match(readLines(top_files[1]), "foo")
  # missing file was downloaded
  expect_true(file.exists(top_files[2]))

  # overwriting restore f1's original content
  out <- osf_download(d2, path = outdir, conflicts = "overwrite")
  expect_match(readLines(top_files[1])[1], f1_text[1])
})
