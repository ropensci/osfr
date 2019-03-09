context("Downloading files")


# setup -------------------------------------------------------------------
setup({
  # Retrieve public OSF project and components required for tests
  # (created using data-raw/create-test-project.R)
  if (on_test_server()) {
    p1 <<- osf_retrieve_node("brfza")
    c1 <<- osf_retrieve_node("rxwhk")
    f1 <<- osf_retrieve_file("ew8tc")
    d1 <<- osf_retrieve_file("5c34b68a44cd030016942349")
  }
  outdir <<- tempdir()
})



# tests -------------------------------------------------------------------
context("Downloading")

test_that("a file can be downloaded from a project", {
  skip_if_no_pat()

  out <- osf_download(f1, path = outdir)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(out$local_path))
})

test_that("an existing file won't be overwritten", {
  skip_if_no_pat()

  expect_error(
    osf_download(f1, path = outdir),
    "A file exists at the specified"
  )
  expect_s3_class(
    osf_download(f1, path = outdir, overwrite = TRUE),
    "osf_tbl_file"
  )
})

test_that("a non-existant path throws an error", {
  skip_if_no_pat()

  expect_error(
    osf_download(f1, path = "ddd"),
    "`path` must point to an existing local directory.")
})

test_that("a file can be downloaded from a directory", {
  skip_if_no_pat()
  f2 <- osf_ls_files(d1, pattern = "image-file-01.png")
  out <- osf_download(f2, path = outdir)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(out$local_path))
})

test_that("a directory can be downloaded as a zip file", {
  skip_if_no_pat()

  zipfile <- file.path(outdir, paste0(d1$name, ".zip"))
  out <- osf_download(d1, path = outdir, decompress = FALSE)
  expect_s3_class(out, "osf_tbl_file")
  expect_true(file.exists(zipfile))
  unlink(zipfile)
})

test_that("a downloaded directory is unzipped", {
  skip_if_no_pat()

  d1_files <- osf_ls_files(d1, n_max = Inf)
  out <- osf_download(d1, path = outdir)
  expect_s3_class(out, "osf_tbl_file")

  expect_true(dir.exists(out$local_path))
  expect_true(all(file.exists(file.path(out$local_path, d1_files$name))))
  unlink(out$local_path, recursive = TRUE)
})

test_that("only missing files are downloaded when ovewrite=FALSE", {
  # create a local version of the first text file in d1
  dir.create(file.path(outdir, d1$name))
  txt_f1 <- file.path(outdir, d1$name, "text-file-01.txt")
  cat("Local file\n", file = txt_f1)

  out <- osf_download(d1, path = outdir)
  expect_match(readLines(txt_f1), "Local file")
})
