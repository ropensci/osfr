context("Moving files")


# setup -------------------------------------------------------------------
vcr::vcr_configure(
  dir = cassette_dir("files")
)

setup({
  infile <<- file.path(tempdir(), "a.txt")
  brio::writeLines("1", con = infile)

  if (has_pat()) {
    vcr::use_cassette("setup-test-project", {
      p1 <<- osf_create_project(title = "osfr-test-files-1")
      p2 <<- osf_create_project(title = "osfr-test-files-2")
      d1 <<- osf_mkdir(p1, "d1")
      d3 <<- osf_mkdir(p2, "d3")
      f1 <<- osf_upload(p1, infile)
    })
  }
})

teardown({
  if (has_pat()) {
    vcr::use_cassette("teardown-test-project", {
      osf_rm(p1, recurse = TRUE, check = FALSE)
      osf_rm(p2, recurse = TRUE, check = FALSE)
    })
  }
})


# tests -------------------------------------------------------------------
test_that("a file can be moved from node to subdirectory and back", {
  skip_if_no_pat()

  vcr::use_cassette("move-f1-to-d1", {
    f1 <- osf_mv(f1, d1)
  })
  expect_match(
    get_meta(f1, "attributes", "materialized_path"),
    file.path(d1$name, basename(infile))
  )

  vcr::use_cassette("move-f1-to-p1", {
    f1 <- osf_mv(f1, p1)
  })
  expect_equal(
    get_meta(f1, "attributes", "materialized_path"),
    paste0("/", basename(infile))
  )
})

test_that("moving respects overwrite argument", {
  skip_if_no_pat()

  vcr::use_cassette("move-conflict-error", {
    f2 <- osf_upload(d1, infile)
    expect_error(
      osf_mv(f1, d1),
      sprintf(
        "Cannot complete action: file or folder \"%s\" already exists",
        basename(infile)
      )
    )
  })

  vcr::use_cassette("move-conflict-overwrite", {
    f1 <- osf_mv(f1, d1, overwrite = TRUE)
    expect_s3_class(f1, "osf_tbl_file")

    expect_match(
      get_meta(f1, "attributes", "materialized_path"),
      file.path(d1$name, f1$name)
    )
  })
})

test_that("moving destination can be a different node", {
  skip_if_no_pat()
  vcr::use_cassette("move-f1-to-p2", {
    f1 <- osf_mv(f1, p2)
  })
  expect_match(get_parent_id(f1), as_id(p2))
})

test_that("directories can be moved to a sibling directory", {
  skip_if_no_pat()
  vcr::use_cassette("move-d2-to-d1", {
    d2 <- osf_mkdir(p1, "d2")
    d1 <- osf_mv(d1, d2)
  })
  expect_match(
    paste0("/", file.path(d2$name, d1$name), "/"),
    get_meta(d1, "attributes", "materialized_path")
  )
})

test_that("moving a parent directory to a child directory errors", {
  skip_if_no_pat()
  vcr::use_cassette("move-parent-to-child", {
    parent <- osf_mkdir(p1, "parent")
    child <- osf_mkdir(parent, "child")
    expect_error(
      osf_mv(parent, child),
      "Can't move a parent directory into its child"
    )
  })
})


context("Copying files")

test_that("a file can copy to new directory", {
  skip_if_no_pat()
  vcr::use_cassette("copy-f1-to-d3", {
    f1 <- osf_refresh(f1)
    f_copy <- osf_cp(f1, d3)
  })
  expect_identical(get_parent_id(f_copy), get_parent_id(f1))
  vcr::use_cassette("delete-f1-copy", {
    osf_rm(f_copy, check = FALSE)
  })
})

test_that("a file cannot copy to same location with either overwrite option", {
  skip_if_no_pat()
  vcr::use_cassette("copy-f1-to-p2", {
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
})

test_that("copy respects overwrite values when copying to a new location", {
  skip_if_no_pat()
  vcr::use_cassette("copy-f1-dupe-to-d3", {
    f1 <- osf_refresh(f1)
    f1_dupe <- osf_cp(f1, d3)
  })

  vcr::use_cassette("copy-f1-to-d3-conflict", {
    expect_error(
      osf_cp(f1_dupe, d3, overwrite = FALSE),
      sprintf(
        "Cannot complete action: file or folder \"%s\" already exists",
        basename(infile)
      )
    )
  })

  vcr::use_cassette("copy-f1-to-d3-overwrite", {
    f1_dupe_2 <- osf_cp(f1, d3, overwrite = TRUE)
  })
  expect_identical(
    get_parent_id(f1_dupe_2),
    get_parent_id(f1)
  )
})


context("Deleting files")

test_that("a single file can be deleted", {
  skip_if_no_pat()
  vcr::use_cassette("delete-f1", {
    f1 <- osf_refresh(f1)
    expect_true(osf_rm(f1, check = FALSE))
  })
})

test_that("an empty directory can be deleted", {
  skip_if_no_pat()
  vcr::use_cassette("delete-d2", {
    d2 <- osf_mkdir(p1, "d2")
    expect_true(
      osf_rm(d2, check = FALSE)
    )
  })
})

test_that("a non-empty directory can be deleted", {
  skip_if_no_pat()
  vcr::use_cassette("delete-d3", {
    d3 <- osf_mkdir(p1, "d1/d2/d3")
    expect_true(
      osf_rm(d3, check = FALSE)
    )
  })
})
