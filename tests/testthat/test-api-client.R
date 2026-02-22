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
