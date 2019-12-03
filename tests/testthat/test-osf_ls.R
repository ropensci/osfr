context("Listing nodes")


# setup -------------------------------------------------------------------
setup({
  # Retrieve public OSF project and components required for tests
  # (created using data-raw/create-test-project.R)
  if (on_test_server()) {
    guids <- get_guids()
    p1 <<- osf_retrieve_node(guids[, "p1"])
    c1 <<- osf_retrieve_node(guids[, "c1"])
    d1 <<- osf_retrieve_file(guids[, "d1"])
  }
})


# tests -------------------------------------------------------------------
test_that("`n_max` controls number of returned nodes", {
  skip_on_production_server()

  out <- osf_ls_nodes(c1, n_max = 10)
  expect_s3_class(out, "osf_tbl_node")
  expect_equal(nrow(out), 10)

  out <- osf_ls_nodes(c1, n_max = 20)
  expect_equal(nrow(out), 20)
})

test_that("`pattern` filters nodes by name", {
  skip_on_production_server()

  out <- osf_ls_nodes(c1, pattern = "component-01")
  expect_equal(nrow(out), 1)

  out <- osf_ls_nodes(c1, pattern = "component-0")
  expect_equal(nrow(out), 9)
})

test_that("messages are printed with `verbose` enabled", {
  skip_on_production_server()

  expect_message(
    osf_ls_nodes(c1, n_max = 20, verbose = TRUE),
    "Retrieving \\d{2} of \\d{2} available items"
  )
})


context("Listing files and directories")

test_that("both files and directories are listed", {
  skip_on_production_server()

  out <- osf_ls_files(p1)
  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 3)
  expect_identical(
    get_meta(out, "attributes", "kind"),
    c("folder", "file", "folder")
  )
})

test_that("`type` can filters for files", {
  skip_on_production_server()

  out <- osf_ls_files(p1, type = "file")
  expect_equal(nrow(out), 1)
  expect_match(get_meta(out, "attributes", "kind"), "file")
})

test_that("`type` can filters for files", {
  skip_on_production_server()

  out <- osf_ls_files(p1, type = "folder")
  expect_equal(nrow(out), 2)
  expect_match(get_meta(out, "attributes", "kind"), "folder")
})

test_that("n_max controls number of returned files", {
  skip_on_production_server()

  out <- osf_ls_files(d1, n_max = 10)
  expect_equal(nrow(out), 10)

  out <- osf_ls_files(d1, n_max = 20)
  expect_equal(nrow(out), 20)
})

test_that("`pattern` filters files by name", {
  skip_on_production_server()

  out <- osf_ls_files(d1, pattern = ".txt", n_max = 10)
  expect_match(out$name, "\\.txt$")

  out <- osf_ls_files(d1, pattern = ".png", n_max = 10)
  expect_match(out$name, "\\.png$")
})

test_that("messages are printed with `verbose` enabled", {
  skip_on_production_server()

  expect_message(
    osf_ls_files(d1, n_max = 20, verbose = TRUE),
    "Retrieving \\d{2} of \\d{2} available items"
  )
})
