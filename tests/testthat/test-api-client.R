# .build_client() ---------------------------------------------------------

test_that("OSF client sets Accept header with version when specified", {
  withr::local_envvar(OSF_SERVER = "test")
  cli <- .build_client(api = "osf", encode = "json", version = "2.20")
  expect_equal(
    cli$headers[["Accept"]],
    "application/vnd.api+json;version=2.20"
  )
})

test_that("OSF client sets Accept header without version by default", {
  withr::local_envvar(OSF_SERVER = "test")
  cli <- .build_client(api = "osf", encode = "json")
  expect_equal(
    cli$headers[["Accept"]],
    "application/vnd.api+json"
  )
})

test_that("Waterbutler client does not set the Accept header", {
  withr::local_envvar(OSF_SERVER = "test")
  cli <- .build_client(api = "wb", encode = "raw")
  expect_null(cli$headers[["Accept"]])
})

test_that("OSF client includes User-Agent header", {
  withr::local_envvar(OSF_SERVER = "test")
  cli <- .build_client(api = "osf", encode = "json")
  expect_match(cli$headers[["User-Agent"]], "^osfr")
})

# OSF_API_VERSION --------------------------------------------------------

test_that("OSF_API_VERSION is defined", {
  expect_type(OSF_API_VERSION, "character")
  expect_match(OSF_API_VERSION, "^\\d+\\.\\d+$")
})

test_that(".osf_request() defaults to the pinned API version", {
  version_default <- formals(.osf_request)$version
  expect_identical(eval(version_default), OSF_API_VERSION)
})
