context("project operations")

login(test_pat)

p1 <- create_project(title = "osfr-project-tests", "Test project operations")

test_that("create projects", {
  expect_error(create_project(), "Specify a project title")
  expect_true(is_valid_osf_id(p1))
})

test_that("get nodes", {
  expect_error(get_nodes(p1, private = FALSE))
  nodes <- get_nodes(p1, private = TRUE)
  expect_is(nodes, "list")
})

test_that("update project assertions", {
  expect_error(update_project(),   "Must specify a node identifier")
  expect_error(update_project(p1), "No updated attribute values specified")
})

test_that("update project title", {
  title <- "osfr-p1-updated"
  p1 <- update_project(p1, title = title)
  p1_attrs <- get_nodes(p1, private = TRUE)$data$attributes
  expect_match(p1_attrs$title, title)
})

test_that("update project privacy", {
  p1 <- update_project(p1, private = FALSE)
  p1_attrs <- get_nodes(p1, private = TRUE)$data$attributes
  expect_true(p1_attrs$public)
})

test_that("project deletion", {
  out <- delete_project(p1)
  expect_match(out, p1)
  expect_error(delete_project(p1), "The requested node is no longer available")
})

