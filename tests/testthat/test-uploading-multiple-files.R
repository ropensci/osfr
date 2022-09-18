context("Uploading multiple files")


# setup -------------------------------------------------------------------
vcr::vcr_configure(
  dir = cassette_dir("uploading-multiple-files")
)

setup({
  # create directory for testing multifile uploads
  multidir <<- create_upload_test_files(".osfr-tests")

  if (has_pat()) {
    vcr::use_cassette("create-p1", {
      p1 <<- osf_create_project(title = "osfr-multifile-upload-test")
    })
  }
})

teardown({
  fs::dir_delete(multidir)
  if (has_pat()) {
    vcr::use_cassette("delete-p1", {
      osf_rm(p1, recurse = TRUE, check = FALSE)
    })
  }
})


# tests -------------------------------------------------------------------

test_that("multiple files can be uploaded", {
  skip_if_no_pat()

  infiles <- fs::dir_ls(multidir, type = "file")

  vcr::use_cassette("upload-infiles", {
    out <- osf_upload(p1, infiles)
  })

  expect_s3_class(out, "osf_tbl_file")
  expect_equal(out$name, basename(infiles))
})

test_that("a directory can be uploaded", {
  skip_if_no_pat()

  vcr::use_cassette("create-and-populate-c1", {
    c1 <- osf_create_component(p1, "dir-upload")
    out <- osf_upload(c1, multidir)
  })
  expect_s3_class(out, "osf_tbl_file")
  expect_match(out$name, basename(multidir))

  # files within the directory were uploaded
  vcr::use_cassette("list-c1-files",
    c1_files <- osf_ls_files(out)
  )
  expect_true(
    all(c1_files$name %in% basename(fs::dir_ls(multidir)))
  )
})

test_that("a subdirectory can be uploaded", {
  skip_if_no_pat()

  # upload the subdirectory with no children
  indir <- file.path(multidir, "subdir1", "subdir1_1")
  vcr::use_cassette("create-and-populate-c2", {
    c2 <- osf_create_component(p1, "subdir-upload")
    out <- osf_upload(c2, indir, verbose = TRUE)
  })

  expect_s3_class(out, "osf_tbl_file")
  expect_match(out$name, basename(indir))

  # verify leaf directory was created in the project root
  vcr::use_cassette("list-c2-subdirs",
    subdirs <- osf_ls_files(c2, type = "folder")
  )
  expect_equal(
    subdirs$name,
    basename(indir)
  )

  # verify files within the directory were uploaded
  vcr::use_cassette("list-c2-subdir-files",
    subdir_files <- osf_ls_files(out)
  )
  expect_equal(
    subdir_files$name,
    basename(fs::dir_ls(indir))
  )
})


test_that("recurse argument respects specified levels", {
  skip_if_no_pat()

  vcr::use_cassette("create-and-populate-c3", {
    c3 <- osf_create_component(p1, "recurse=1")
    out <- osf_upload(c3, path = multidir, recurse = 1)
  })

  vcr::use_cassette("list-c3-subdir1",
    sd1_files <- osf_ls_files(out, path = "subdir1")
  )
  expect_setequal(
    sort(sd1_files$name),
    dir(file.path(multidir, "subdir1"))
  )

  vcr::use_cassette("list-c3-subdir2",
    sd2_files <- osf_ls_files(out, path = "subdir2")
  )
  expect_setequal(
    sd2_files$name,
    dir(file.path(multidir, "subdir2"))
  )

  vcr::use_cassette("list-c3-subdir1_1",
    sd1_1_files <- osf_ls_files(out, path = "subdir1/subdir1_1")
  )
  expect_equal(nrow(sd1_1_files), 0)
})


test_that("recurse=TRUE uploads the entire OSF directory structure", {
  skip_if_no_pat()

  vcr::use_cassette("create-and-populate-c4", {
    c4 <- osf_create_component(p1, "recurse=TRUE")
    out <- osf_upload(c4, path = multidir, recurse = TRUE)
  })

  vcr::use_cassette("list-c4-subdir1_1-files",
    sd1_1_files <- osf_ls_files(out, path = "subdir1/subdir1_1")
  )
  expect_equal(
    sd1_1_files$name,
    dir(file.path(multidir, "subdir1/subdir1_1"))
  )
})

test_that("files in parent directories can be uploaded", {
  # set working directory a test subdirectory
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(fs::path(multidir, "subdir1/subdir1_1"))
  skip_if_no_pat()

  vcr::use_cassette("create-c5", {
    c5 <- osf_create_component(p1, "parent-directories")
  })

  vcr::use_cassette("c5-upload-f1", {
    upf1 <- osf_upload(c5, path = "../d.txt")
  })
  vcr::use_cassette("c5-upload-f2", {
    upf2 <- osf_upload(c5, path = "../../a.txt")
  })
  vcr::use_cassette("c5-upload-updir", {
    updir <- osf_upload(c5, path = "../../subdir2")
  })

  vcr::use_cassette("list-c5-files",
    c5_files <- osf_ls_files(c5)
  )
  expect_equal(
    c5_files$name,
    c("d.txt", "a.txt", "subdir2")
  )
})

test_that("conflicting files are skipped or overwritten", {
  skip_if_no_pat()

  infiles <- fs::dir_ls(multidir, type = "file")
  vcr::use_cassette("create-and-populate-c6", {
    c6 <- osf_create_component(p1, "conflicts")
    out1 <- osf_upload(c6, path = infiles[1])
  })

  # overwrite contents of first file
  brio::writeLines("foo", infiles[1])
  vcr::use_cassette("c6-conflict-skip", {
    out2 <- osf_upload(c6, path = infiles[1], conflicts = "skip")
  })

  # all files are still on version 1
  versions <- get_meta(out2, "attributes", "current_version")
  expect_true(all(versions) == 1)

  # overwriting with new file increments version
  vcr::use_cassette("c6-conflict-overwrite", {
    out3 <- osf_upload(c6, path = infiles[1], conflicts = "overwrite")
  })
  expect_equal(get_meta(out3, "attributes", "current_version"), 2)
})
