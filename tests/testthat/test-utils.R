context("Recursion utilities")

tree <- list(
  n0 = list(
    n01 = list(
      n011 = "n011",
      n012 = list(
        n0121 = "n0121"
      )
    ),
    n02 = list(
      n021 = "n021",
      n022 = "n022"
    ),
    n03 = "n03"
  )
)

test_that("tree coercion to character vector", {
  stree <- simplify_tree(tree)
  expect_length(stree, 9)
  expect_false(any(duplicated(stree)))
})

test_that("coerce tree leaf", {
  expect_error(simplify_tree(tree$n0$n03), "is.list\\(x\\) is not TRUE")
  expect_equal(
    simplify_tree(tree$n0["n03"]),
    c(n03 = "n03")
  )
})


context("File system utilities")

test_that("Reserved filenames are removed from osf-bound file paths", {
  expect_match(clean_osf_path("."),          ".")
  expect_match(clean_osf_path("/"),          ".")
  expect_match(clean_osf_path("../data"),    "data")
  expect_match(clean_osf_path("../../data"), "data")
  expect_match(clean_osf_path("./data"),     "data")
})


context("URL Utilities")

test_that("url_path() doesn't drop trailing slashes", {
  expect_equal(url_path("a/"), "a/")
})

test_that("url_path() removes multiple slashes", {
  expect_equal(url_path("a", "b"), "a/b")
  expect_equal(url_path("a/", "/b"), "a/b")
  expect_equal(url_path("a/", "/", "/b"), "a/b")
})
