context("Component operations")


# setup -------------------------------------------------------------------
p1 <- osf_project_create(title = "osfr-component-tests")
c1 <- osf_component_create(p1, title = "component-1")


# tests -------------------------------------------------------------------
test_that("create component", {
  expect_error(osf_component_create(), "Must specify ID of a parent project")
  expect_s3_class(c1, "osf_tbl_node")
  expect_match(c1$id, osf_node_ls(p1)$id[1])
})

test_that("update component title", {
  title <- "component-1-updated"
  c1 <- osf_component_update(c1, title = title)
  c1_attrs <- c1$meta[[1]]$attributes
  expect_match(c1_attrs$title, title)
})

test_that("add nested components", {
  c11 <- osf_component_create(c1, title = "component-1-1")
  expect_s3_class(c11, "osf_tbl_node")
  c12 <- osf_component_create(c1, title = "component-1-2")
  expect_s3_class(c12, "osf_tbl_node")
})

test_that("deleting non-empty project/component fails", {
  expect_error(osf_project_delete(p1), "Any child components must be deleted")
  expect_error(osf_project_delete(c1), "Any child components must be deleted")
})

test_that("non-empty project can be recursively deleted", {
  out <- osf_project_delete(p1, recursive = TRUE)
  expect_true(out)
})
