context("project operations")

login(osf_pat)

# public project
p1 <- create_project(title = 'osfr-p1', description = 'Text', private = FALSE)
# private project
p2 <- create_project(title = 'osfr-p2')

test_that("create projects", {
  expect_error(create_project(), regexp = 'Specify a project title')
  expect_true(is_valid_osf_id(p1))
  expect_true(is_valid_osf_id(p2))
})

test_that("create components", {
  c1 <- create_component(p1, title = 'osfr-component')
  expect_true(is_valid_osf_id(c1))
})

# view_project not tested
# won't be implemented either.

test_that("update project", {
  expect_error(update_project())
  expect_true(update_project(p1, private = TRUE))
})

# test_that("clone project", {
#   expect_error(clone_project())
#   expect_true(clone_project(p1))
# })

test_that("delete projects", {
  expect_error(delete_project())
  expect_true(delete_project(p2))

  expect_error(delete_project(p1))
  expect_true(delete_project(p1, recursive = TRUE))
})
