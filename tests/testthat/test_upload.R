context("Test upload functions")

test_that("Uploading comments", {
  expect_error(comment_osf())
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  Sys.getenv("OSF_PAT")
  # Forcing as.character to make test consistent across machines
  expect_that(as.character(comment_osf(id = "djxwq", txt = "Tesdsat", test = TRUE)$status_code),
              matches("201"))
  logout()
})

test_that("Uploading files", {
  expect_error(upload_osf())
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  expect_error(upload_osf(id = 'kjh47'))
  getwd()
  expect_true(upload_revision(id = 'kjh47', file = 'test_upload1', test = TRUE))
  # expect_that(upload_osf(id = 'djxwq'))
  logout()
})
