
# https://github.com/jimhester/lintr
if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("package style", {
    lintr::expect_lint_free(cache = TRUE)
  })
}

# lintr::lint_package()
# covr::package_coverage()
