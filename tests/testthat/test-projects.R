context("project operations")

# test_that("create project", {
#   expect_error(create_project())
#   expect_true(grepl(create_project(title = 'Test',
#      description = 'Text', private = FALSE),
#       pattern = '[A-Za-z0-9]{5}'))
#   expect_true(grepl(create_project(title = 'Test'),
#     pattern = '[A-Za-z0-9]{5}'))
#   expect_true(grepl(create_project(title = 'Test',
#      description = 'Text'), pattern = '[A-Za-z0-9]{5}'))
# })

# # view_project not tested
# # won't be implemented either

# x <- create_project('tmp')

# test_that("update project", {
  
#   expect_error(update_project())
#   expect_true(update_project(x, private = FALSE))
# })

# test_that("clone project", {
#   expect_error(clone_project())
#   expect_true(clone_project(x))
# })

# test_that("delete project", {
#   expect_error(delete_project())
#   # add a node for recursive
#   create_component(x, title = 'Test')
#   expect_error(delete_project(x))
#   expect_true(delete_project(x, recursive = TRUE))
# })