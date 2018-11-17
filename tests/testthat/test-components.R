context("component operations")


# setup -------------------------------------------------------------------
p1 <- osf_project(title = "osfr-component-tests")
c1 <- create_component(p1, title = "component-1")



# tests -------------------------------------------------------------------
test_that("create component", {
  expect_error(create_component(), "Specify ID of a parent project")
  expect_true(is_valid_osf_id(c1))
  expect_match(c1, node_children(p1)[1])
})

test_that("update component title", {
  title <- "component-1-updated"
  c1 <- update_component(c1, title = title)
  c1_attrs <- get_nodes(c1, private = TRUE)$data$attributes
  expect_match(c1_attrs$title, title)
})

test_that("add nested components", {
  c11 <- create_component(c1, title = "component-1-1")
  expect_true(is_valid_osf_id(c11))
  c12 <- create_component(c1, title = "component-1-2")
  expect_true(is_valid_osf_id(c12))
})

test_that("deleting non-empty project/component fails", {
  expect_error(delete_project(p1, "Unable to delete node"))
  expect_error(delete_component(c1, "Unable to delete node"))
})

test_that("non-empty project can be recursively deleted", {
  out <- delete_project(p1, recursive = TRUE)
  expect_match(out, p1)
})
