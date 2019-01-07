context("Project operations")

p1 <- osf_create_project(title = "osfr-project-tests")

test_that("create project", {
  expect_error(osf_create_project(), "Must define a title")
  expect_s3_class(p1, "osf_tbl_node")
})

test_that("project deletion", {
  expect_true(osf_rm(p1))
  out <- .osf_node_retrieve(p1$id)
  expect_equal(out$status_code, 410)
})
