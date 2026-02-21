# setup -------------------------------------------------------------------

vcr::vcr_configure(
  dir = cassette_dir("html-entity-encoding")
)

ugly_name <- "> data & < files"
vcr::use_cassette("create-p1", {
  p1 <- osf_create_project(title = "osfr-test-html-entities")
  c1 <- osf_create_component(p1, title = ugly_name)
})

withr::defer(
  if (has_pat()) try(osf_rm(p1, recurse = TRUE, check = FALSE), silent = TRUE),
  testthat::teardown_env()
)

# tests -------------------------------------------------------------------

test_that("html symbols are decoded when creating nodes", {
  expect_match(c1$name, ugly_name)
})

test_that("html symbols are decoded when retrieving nodes", {
  vcr::use_cassette("retrieve-p1", {
    c2 <- osf_retrieve_node(as_id(c1))
  })
  expect_match(c2$name, ugly_name)
})

test_that("html symbols are encoded when searching nodes", {
  on.exit(vcr::eject_cassette())

  vcr::insert_cassette("search-with-html-symbols")
  expect_equal(nrow(osf_ls_nodes(p1, pattern = "&")), 1)
  expect_equal(nrow(osf_ls_nodes(p1, pattern = "<")), 1)
  expect_equal(nrow(osf_ls_nodes(p1, pattern = ">")), 1)
})
