context("project operations")

login(test_pat)

p1 <- osf_project(title = "osfr-project-tests", description = "Is good.")

test_that("create project", {
  expect_error(osf_project(), "Provide an ID to retrieve a project")
  expect_s3_class(p1, "osf_tbl_node")
})

test_that("get nodes", {
  expect_error(get_nodes(p1$id, private = FALSE))
  nodes <- get_nodes(p1$id, private = TRUE)
  expect_is(nodes, "list")
})

# test_that("update project assertions", {
#   expect_error(update_project(),   "Must specify a node identifier")
#   expect_error(update_project(p1), "No updated attribute values specified")
# })

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

