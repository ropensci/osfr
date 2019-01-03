context("Project operations")

p1 <- osf_create_project(title = "osfr-project-tests")

test_that("create project", {
  expect_error(osf_create_project(), "Must define a title")
  expect_s3_class(p1, "osf_tbl_node")
})

test_that("update project assertions", {
  expect_error(osf_project_update(), "Must specify ID of a project to update")
  expect_error(osf_project_update(p1), "No updated attribute values specified")
})

test_that("update project title", {
  title <- "osfr-p1-updated"
  p1 <- osf_project_update(p1, title = title)

  p1_attrs <- p1$meta[[1]]$attributes
  expect_match(p1_attrs$title, title)
})

test_that("update project privacy", {
  p1 <- osf_project_update(p1, private = FALSE)
  p1_attrs <- p1$meta[[1]]$attributes
  expect_true(p1_attrs$public)
})

test_that("project deletion", {
  expect_true(osf_rm(p1))
  out <- .osf_node_retrieve(p1$id)
  expect_equal(out$status_code, 410)
})
