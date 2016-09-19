context("Test downloading")

test_that("copying is not theft", {
  expect_that(download.osf(id = 'bzm73', file = 'test.file.remove', test = TRUE)$status_code, matches("200"))
  expect_that(download.osf(id = 'bzm73', test = TRUE)$status_code, matches("200"))
  # Set the test PAT before testing
  # Make sure to have object OSF_PAT_TEST in environment
  # I usually keep it in ~/.Rprofile
  Sys.setenv(OSF_PAT = Sys.getenv("OSF_PAT_TEST"))
  expect_that(download.osf(id = '2pf6w', file = 'test.file.remove', test = TRUE,
                           private = TRUE)$status_code, matches("200"))
  logout()
})
