context("Component operations")


# setup -------------------------------------------------------------------
p1 <- osf_create_project(title = "osfr-component-tests")
c1 <- osf_create_component(p1, title = "component-1")


# tests -------------------------------------------------------------------
test_that("create component", {
  expect_error(osf_create_component(), "`x` must be an `osf_tbl_node`")
  expect_s3_class(c1, "osf_tbl_node")
  expect_match(c1$id, osf_ls_nodes(p1)$id[1])
})

test_that("add nested components", {
  c11 <- osf_create_component(c1, title = "component-1-1")
  expect_s3_class(c11, "osf_tbl_node")
  c12 <- osf_create_component(c1, title = "component-1-2")
  expect_s3_class(c12, "osf_tbl_node")
})

test_that("deleting non-empty project/component fails", {
  expect_error(osf_rm(p1), "Any child components must be deleted")
  expect_error(osf_rm(c1), "Any child components must be deleted")
})

test_that("non-empty project can be recursively deleted", {
  out <- osf_rm(p1, recursive = TRUE)
  expect_true(out)
})
