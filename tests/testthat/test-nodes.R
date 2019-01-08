context("Node creation")

p1 <- osf_create_project(title = "osfr-project-tests")

test_that("minimal project with default settings was created", {
  expect_s3_class(p1, "osf_tbl_node")
  expect_false(get_meta(p1, "attributes", "public"))
  expect_match(p1$name, "osfr-project-tests")
  expect_null(get_parent_id(p1))
})

c1 <- osf_create_component(p1, title = "component-1")

test_that("minimal component with default settings was created", {
  expect_s3_class(c1, "osf_tbl_node")
  expect_false(get_meta(c1, "attributes", "public"))
  expect_match(c1$name, "component-1")
  expect_match(get_parent_id(c1), p1$id)
})

test_that("node creation errors without a title", {
  expect_error(osf_create_project(), "Must define a title")
  expect_error(osf_create_component(p1), "Must define a title")
})

test_that("component creation errors with providing a parent node", {
  expect_error(osf_create_component(), "`x` must be an `osf_tbl_node`")
})

test_that("nested nodes can be created", {
  c11 <- osf_create_component(c1, title = "component-1-1")
  expect_s3_class(c11, "osf_tbl_node")
  expect_match(get_parent_id(c11), c1$id)

  c12 <- osf_create_component(c1, title = "component-1-2")
  expect_s3_class(c12, "osf_tbl_node")
  expect_match(get_parent_id(c12), c1$id)
})


context("Node deletion")

test_that("deleting non-empty project/component fails", {
  expect_error(osf_rm(p1), "Any child components must be deleted")
  expect_error(osf_rm(c1), "Any child components must be deleted")
})

test_that("non-empty project can be recursively deleted", {
  out <- osf_rm(p1, recursive = TRUE)
  expect_true(out)
})
