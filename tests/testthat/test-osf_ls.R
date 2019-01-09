context("Listing nodes")

# Retrieve public OSF project and components required for tests
# (created using data-raw/create-test-project.R)
test_proj <- osf_retrieve_node("brfza")
test_comp <- osf_retrieve_node("rxwhk")

test_that("`n_max` controls number of returned nodes", {
  out <- osf_ls_nodes(test_comp, n_max = 10)
  expect_s3_class(out, "osf_tbl_node")
  expect_equal(nrow(out), 10)

  out <- osf_ls_nodes(test_comp, n_max = 20)
  expect_equal(nrow(out), 20)
})

test_that("`pattern` filters nodes by name", {
  out <- osf_ls_nodes(test_comp, pattern = "component-01")
  expect_equal(nrow(out), 1)

  out <- osf_ls_nodes(test_comp, pattern = "component-0")
  expect_equal(nrow(out), 9)
})

test_that("messages are printed with `verbose` enabled", {
  expect_message(
    osf_ls_nodes(test_comp, n_max = 20, verbose = TRUE),
    "Retrieving \\d{2} of \\d{2} available items"
  )
})


context("Listing files and directories")

test_that("both files and directories are listed", {
  out <- osf_ls_files(test_proj)
  expect_s3_class(out, "osf_tbl_file")
  expect_equal(nrow(out), 2)
  expect_identical(
    get_meta(out, "attributes", "kind"),
    c("folder", "file")
  )
})

test_that("`type` can filters for files", {
  out <- osf_ls_files(test_proj, type = "file")
  expect_equal(nrow(out), 1)
  expect_match(get_meta(out, "attributes", "kind"), "file")
})

test_that("`type` can filters for files", {
  out <- osf_ls_files(test_proj, type = "folder")
  expect_equal(nrow(out), 1)
  expect_match(get_meta(out, "attributes", "kind"), "folder")
})


test_dir <- osf_ls_files(test_proj, type = "folder")

test_that("n_max controls number of returned files", {
  out <- osf_ls_files(test_dir, n_max = 10)
  expect_equal(nrow(out), 10)

  out <- osf_ls_files(test_dir, n_max = 20)
  expect_equal(nrow(out), 20)
})

test_that("`pattern` filters files by name", {
  out <- osf_ls_files(test_dir, pattern = ".txt", n_max = 10)
  expect_match(out$name, "\\.txt$")

  out <- osf_ls_files(test_dir, pattern = ".png", n_max = 10)
  expect_match(out$name, "\\.png$")
})

test_that("messages are printed with `verbose` enabled", {
  expect_message(
    osf_ls_files(test_dir, n_max = 20, verbose = TRUE),
    "Retrieving \\d{2} of \\d{2} available items"
  )
})
