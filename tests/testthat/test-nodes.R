context("Node creation")


# setup -------------------------------------------------------------------

vcr::vcr_configure(
  dir = cassette_dir("nodes")
)

setup({
  if (has_pat()) {
    vcr::use_cassette("create-p1", {
      p1 <<- osf_create_project(title = "osfr-test-nodes")
    })
  }
})

# teardown happens within node deletion tests


# tests -------------------------------------------------------------------
test_that("minimal project with default settings was created", {
  skip_if_no_pat()

  expect_s3_class(p1, "osf_tbl_node")
  expect_false(get_meta(p1, "attributes", "public"))
  expect_match(p1$name, "osfr-test-nodes")
  expect_null(get_parent_id(p1))
})

test_that("minimal component with default settings was created", {
  skip_if_no_pat()

  vcr::use_cassette("create-c1", {
    c1 <<- osf_create_component(p1, title = "component-1")
  })

  expect_s3_class(c1, "osf_tbl_node")
  expect_false(get_meta(c1, "attributes", "public"))
  expect_match(c1$name, "component-1")
  expect_match(get_parent_id(c1), p1$id)
})

test_that("node creation errors without a title", {
  skip_if_no_pat()

  expect_error(osf_create_project(), "Must define a title")
  expect_error(osf_create_component(p1), "Must define a title")
})

test_that("component creation errors with providing a parent node", {
  skip_if_no_pat()
  expect_error(osf_create_component(), "`x` must be an `osf_tbl_node`")
})

test_that("nested nodes can be created", {
  skip_if_no_pat()

  vcr::use_cassette("create-c11", {
    c11 <- osf_create_component(c1, title = "component-1-1")
  })
  expect_s3_class(c11, "osf_tbl_node")
  expect_match(get_parent_id(c11), c1$id)

  vcr::use_cassette("create-c12", {
    c12 <- osf_create_component(c1, title = "component-1-2")
  })
  expect_s3_class(c12, "osf_tbl_node")
  expect_match(get_parent_id(c12), c1$id)
})


context("Node categories")

test_that("default project category is 'project'", {
  skip_if_no_pat()
  expect_match(get_meta(p1, "attributes", "category"), "project")
})

test_that("default component category is empty (i.e., uncategorized)", {
  skip_if_no_pat()
  expect_match(get_meta(c1, "attributes", "category"), "")
})

test_that("an invalid or ambiguous category errors", {
  skip_if_no_pat()

  expect_error(osf_create_project("Bad category", category = "pr"))
  expect_error(osf_create_component(p1, "Bad category", category = "pr"))
})

test_that("a valid category can be specified", {
  skip_if_no_pat()

  vcr::use_cassette("create-p2-and-c2", {
    p2 <<- osf_create_project("osfr-project-category-test", category = "Analysis")
    c2 <- osf_create_component(p2, "osfr-component-category-test", category = "Data")
  })

  expect_match(get_meta(p2, "attributes", "category"), "analysis")
  expect_match(get_meta(c2, "attributes", "category"), "data")
})


context("Node deletion")

test_that("deleting non-empty project/component fails", {
  skip_if_no_pat()

  expect_error(
    vcr::use_cassette("delete-p1-error", {
      osf_rm(p1, check = FALSE)
    }),
    "Any child components must be deleted"
  )
  expect_error(
    vcr::use_cassette("delete-c1-error", {
      osf_rm(c1, check = FALSE)
    }),
    "Any child components must be deleted"
  )
})

test_that("non-empty project can be recursively deleted", {
  skip_if_no_pat()

  vcr::use_cassette("delete-p1-success", {
    expect_true(osf_rm(p1, recurse = TRUE, check = FALSE))
  })

  vcr::use_cassette("delete-p2-success", {
    expect_true(osf_rm(p2, recurse = TRUE, check = FALSE))
  })
})
