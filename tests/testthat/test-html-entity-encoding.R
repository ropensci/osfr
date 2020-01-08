
# setup -------------------------------------------------------------------

setup({
  if (has_pat()) {
    p1 <<- osf_create_project(title = "osfr-test-html-entities")

    ugly_name <<- "> data & < files"
    c1 <<- osf_create_component(p1, title = ugly_name)
  }
})

teardown({
  if (has_pat()) {
    osf_rm(p1, recursive = TRUE, check = FALSE)
  }
})


# tests -------------------------------------------------------------------

test_that("html symbols are decoded when creating nodes", {
  skip_if_no_pat()
  expect_match(c1$name, ugly_name)
})

test_that("html symbols are decoded when retrieving nodes", {
  skip_if_no_pat()
  c2 <- osf_retrieve_node(as_id(c1))
  expect_match(c2$name, ugly_name)
})

test_that("html symbosl are encoded when searching nodes", {
  skip_if_no_pat()
  expect_equal(nrow(osf_ls_nodes(p1, pattern = "&")), 1)
  expect_equal(nrow(osf_ls_nodes(p1, pattern = "<")), 1)
  expect_equal(nrow(osf_ls_nodes(p1, pattern = ">")), 1)
})
