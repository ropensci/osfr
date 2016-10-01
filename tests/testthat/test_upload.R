context("Test upload functions")

test_that("Uploading comments", {
  expect_error(comment.osf())
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  Sys.getenv("OSF_PAT")
  # Forcing as.character to make test consistent across machines
  expect_that(as.character(comment.osf(id = "djxwq", txt = "Tesdsat", test = TRUE)$status_code),
              matches("201"))
  logout()
})

test_that("Uploading files", {
  expect_error(upload.osf())
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  expect_error(upload.osf(id = 'kjh47'))
  # expect_that(upload.osf(id = 'djxwq'))
  logout()
})
