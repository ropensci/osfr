context("Test upload functions")

Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))

test_that("Uploading comments", {
  expect_error(comment())
  # Forcing as.character to make test consistent across machines
  expect_true(comment(id = "djxwq", txt = "Tesdsat", test = TRUE))
})

# test_that("Uploading files", {
#   expect_error(upload_osf())
#   Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
#   expect_error(upload_osf(id = 'kjh47'))
#   getwd()
#   expect_true(upload_revision(id = 'kjh47', filename = 'test_upload1.png', test = TRUE))
#
#   # expect_true(upload_osf(id = 'djxwq', filename = 'test_upload1.png', test = TRUE))
#   # expect_true(upload_osf(id = 'djxwq', filename = 'test_upload1.png', test = TRUE))
#   logout()
# })

logout()
