# raise_error() auth messages -----------------------------------------------

test_that("raise_error() provides hint for 401 responses", {
  err <- list(
    errors = list(list(detail = "Authentication credentials were not provided.")),
    status_code = 401L
  )
  expect_error(
    raise_error(err),
    "Authentication failed"
  )
  expect_error(
    raise_error(err),
    "osf_auth"
  )
})

test_that("raise_error() provides hint for 403 responses", {
  err <- list(
    errors = list(list(detail = "You do not have permission to perform this action.")),
    status_code = 403L
  )
  expect_error(
    raise_error(err),
    "Authentication failed"
  )
  expect_error(
    raise_error(err),
    "required scopes"
  )
})

test_that("raise_error() includes API detail in auth error messages", {
  detail <- "Authentication credentials were not provided."
  err <- list(
    errors = list(list(detail = detail)),
    status_code = 401L
  )
  expect_error(
    raise_error(err),
    detail,
    fixed = TRUE
  )
})

test_that("raise_error() passes through non-auth errors to http_error()", {
  err <- list(
    errors = list(list(detail = "Not found.")),
    status_code = 404L
  )
  expect_error(
    raise_error(err),
    "HTTP status code 404"
  )
})

test_that("raise_error() includes detail in non-auth error messages", {
  err <- list(
    errors = list(list(detail = "Not found.")),
    status_code = 404L
  )
  expect_error(
    raise_error(err),
    "Not found.",
    fixed = TRUE
  )
})

test_that("raise_error() is silent when there are no errors", {
  expect_silent(raise_error(list(data = list())))
})
